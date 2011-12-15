#lang racket

(require rackunit)

;; The only visible export of this module will be parse-expr.
(provide parse-expr)

;; parse-expr: any input-port -> (U syntax eof)
;; Either produces a syntax object or the eof object.
(define (parse-expr src in)
  (define-values (line column position) (port-next-location in))
  (define next-char (read-char in))
  
  ;; decorate/span: s-expression number -> syntax
  ;; Wrap the s-expression with source location.
  (define (decorate sexp span)
    (datum->syntax #f sexp (list src line column position span)))

  (cond
    [(eof-object? next-char) eof]
    [else
     (case next-char
       [(#\<) (decorate '(less-than) 1)]
       [(#\>) (decorate '(greater-than) 1)]
       [(#\+) (decorate '(plus) 1)]
       [(#\-) (decorate '(minus) 1)]
       [(#\,) (decorate '(comma) 1)]
       [(#\.) (decorate '(period) 1)]
       [(#\[)
        ;; The slightly messy case is bracket.  We keep reading
        ;; a list of exprs, and then construct a wrapping bracket
        ;; around the whole thing.
        (define elements (parse-exprs src in))
        (define-values (l c tail-position) 
          (port-next-location in))
        (decorate `(brackets ,@elements)
                  (- tail-position position))]
       [else
        (parse-expr src in)])]))

;; parse-exprs: input-port -> (listof syntax)
;; Parse a list of expressions.
(define (parse-exprs source-name in)
  (define peeked-char (peek-char in))
  (cond
    [(eof-object? peeked-char)
     (error 'parse-exprs "Expected ], but read eof")]
    [(char=? peeked-char #\])
     (read-char in)
     empty]
    [(member peeked-char (list #\< #\> #\+ #\- #\, #\. #\[))
     (cons (parse-expr source-name in)
           (parse-exprs source-name in))]
    [else
     (read-char in)
     (parse-exprs source-name in)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; simple tests
(check-equal? eof (parse-expr 'test (open-input-string "")))
(check-equal? '(greater-than) 
              (syntax->datum (parse-expr 'test (open-input-string ">"))))
(check-equal? '(less-than) 
              (syntax->datum (parse-expr 'test (open-input-string "<"))))
(check-equal? '(plus) 
              (syntax->datum (parse-expr 'test (open-input-string "+"))))
(check-equal? '(minus)
              (syntax->datum (parse-expr 'test (open-input-string "-"))))
(check-equal? '(comma)
              (syntax->datum (parse-expr 'test (open-input-string ","))))
(check-equal? '(period)
              (syntax->datum (parse-expr 'test (open-input-string "."))))


;; bracket tests
(check-equal? '(brackets) 
              (syntax->datum (parse-expr 'test (open-input-string "[]"))))
(check-equal? '(brackets (brackets))
              (syntax->datum (parse-expr 'test (open-input-string "[[]]"))))


;; Parsing the "cat" function
(let ([port (open-input-string ",[.,]")])
  (check-equal? '(comma) 
                (syntax->datum (parse-expr 'test port)))
  (check-equal? '(brackets (period) (comma))
                (syntax->datum (parse-expr 'test port)))
  (check-equal? eof
                (parse-expr 'test port)))