#lang racket/base


(require racket/list
         rackunit)


;; http://en.wikipedia.org/wiki/Brainfuck
;;
;; The surface syntax of Brainfudge consists of the following
;; characters:
;;
;; >
;; <
;; +
;; -
;; .
;; ,
;; [
;; ]
;;
;; and we ignore everything else.



;; This module takes programs written in the surface syntax, and
;; translates them to Racket syntax objects.  See reader.rkt to see
;; how the parser is used.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following is a simple recursive-descent parser.  We might want
;; to do something more sophisticated, using Racket's parser-tools
;; collection, for example.
;;
;; http://docs.racket-lang.org/parser-tools/index.html
;;
;; One of the advantages of the parser tools is that it should help
;; maintain source locations within their own token structure.  In the
;; baby parser, we're only caring about the textual lexeme, and not
;; where it occurs in the input port.



;; A token stream represents the list of tokens we can get from the
;; system.
(define-struct tstream (elts) #:mutable)


;; We want two features from our token stream: peek and pull out new
;; tokens.


;; get-tokens: input-port -> tstream
;; Constructs a new tstream from an input port.
(define (get-tokens in)
  (make-tstream
   (let loop ()
     (let ([next-char (read-char in)])
       (cond
        [(eof-object? next-char)
         empty]
        [(member next-char '(#\> #\< #\+ #\- #\. #\, #\[ #\]))
         (cons next-char (loop))]
        [else
         (loop)])))))


;; next: tstream -> (U token eof)
;; Produces the next element of the stream.
(define (next a-tstream)
  (cond
   [(empty? (tstream-elts a-tstream))
    eof]
   [else
    (let ([next-token (first (tstream-elts a-tstream))])
      (set-tstream-elts! a-tstream (rest (tstream-elts a-tstream)))
      next-token)]))


;; peek: tstream -> (U token eof)
;; Peeks at the next element of the stream, but doesn't consume it.
(define (peek a-tstream)
  (cond
   [(empty? (tstream-elts a-tstream))
    eof]
   [else
    (first (tstream-elts a-tstream))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tests!

;; Let's try a few test cases to make sure the tokenization is doing
;; the right thing.
(let ([a-tstream (get-tokens (open-input-string "<>"))])
  (check-equal? (next a-tstream) #\<)
  (check-equal? (next a-tstream) #\>)
  (check-equal? (next a-tstream) eof)
  (check-equal? (next a-tstream) eof))

;; Make sure peek is doing something reasonable.
(let ([a-tstream (get-tokens (open-input-string "<>"))])
  (check-equal? (peek a-tstream) #\<)
  (check-equal? (peek a-tstream) #\<)
  (check-equal? (next a-tstream) #\<)
  (check-equal? (next a-tstream) #\>)
  (check-equal? (next a-tstream) eof)
  (check-equal? (next a-tstream) eof))

(let ([a-tstream (get-tokens (open-input-string ""))])
  (check-equal? (peek a-tstream) eof)
  (check-equal? (next a-tstream) eof))

(let ([a-tstream (get-tokens (open-input-string " [ ]  +  -  .   ,"))])
  (check-equal? (next a-tstream) #\[)
  (check-equal? (next a-tstream) #\])
  (check-equal? (next a-tstream) #\+)
  (check-equal? (next a-tstream) #\-)
  (check-equal? (next a-tstream) #\.)
  (check-equal? (next a-tstream) #\,)
  (check-equal? (next a-tstream) eof))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Now let's work on the parser.
;;
;;
;; This module will take input port with a program, and produce an AST
;; using Racket's native syntax objects.
;;
;; AST  :== (toplevel EXPR ...)
;;
;; EXPR :==  (increment-data-pointer)
;;         | (decrement-data-pointer)
;;         | (increment-byte)
;;         | (decrement-byte)
;;         | (output-byte)
;;         | (accept-byte)
;;         | (loop EXPR ...)



;; parse-expr: input-port -> syntax-object
(define (parse-expr a-tstream)
  (let ([next-token (next a-tstream)])
    (cond
     [(eof-object? next-token)
      (error 'parse-expr "unexpected eof")]
     [else
      (case next-token
        [(#\>)
         (datum->syntax #f '(increment-data-pointer))]
        [(#\<)
         (datum->syntax #f '(decrement-data-pointer))]
        [(#\+)
         (datum->syntax #f '(increment-byte))]
        [(#\-)
         (datum->syntax #f '(decrement-byte))]
        [(#\.)
         (datum->syntax #f '(output-byte))]
        [(#\,)
         (datum->syntax #f '(accept-byte))]
        [(#\[)
         (let ([inner-exprs (parse-exprs a-tstream)])
           (unless (char=? (next a-tstream) #\])
             (error 'parse-expr "Expected ']"))
           (datum->syntax #f (cons 'loop inner-exprs)))]
        [else
         (error 'parse-expr)])])))

;; parse-exprs: tstream -> (listof syntax-object)
;;
;; Tries to parse as many exprs as possible, till we hit either eof or
;; a #\].
(define (parse-exprs a-tstream)
  (let ([peeked-token (peek a-tstream)])
    (cond
     [(eof-object? peeked-token)
      empty]
     [(char=? peeked-token #\])
      empty]
     [else
      (let ([next-expr (parse-expr a-tstream)])
        (cons next-expr
              (parse-exprs a-tstream)))])))


(define (parse-toplevel a-tstream)
  (datum->syntax #f (cons 'toplevel 
                          (parse-exprs a-tstream))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; simple test
(let ([tstream (get-tokens (open-input-string "<>+-.,"))])
  (check-equal? (syntax->datum (parse-expr tstream))
                '(decrement-data-pointer))
  (check-equal? (syntax->datum (parse-expr tstream))
                '(increment-data-pointer))
  (check-equal? (syntax->datum (parse-expr tstream))
                '(increment-byte))
  (check-equal? (syntax->datum (parse-expr tstream))
                '(decrement-byte))
  (check-equal? (syntax->datum (parse-expr tstream))
                '(output-byte))
  (check-equal? (syntax->datum (parse-expr tstream))
                '(accept-byte))
  (check-exn exn:fail? (lambda () (parse-expr tstream))))


;; Let's try some loops.
(let ([tstream (get-tokens (open-input-string "[+-]"))])
  (check-equal? (syntax->datum (parse-expr tstream))
                '(loop (increment-byte) (decrement-byte)))
  (check-exn exn:fail? (lambda () (parse-expr tstream))))

(let ([tstream (get-tokens (open-input-string "[>[+-]<]"))])
  (check-equal? (syntax->datum (parse-expr tstream))
                '(loop (increment-data-pointer)
                       (loop (increment-byte)
                             (decrement-byte))
                       (decrement-data-pointer)))
  (check-exn exn:fail? (lambda () (parse-expr tstream))))


;; If we unbalance, we expect to see exceptions.
(check-exn exn:fail? (lambda () (parse-expr (get-tokens (open-input-string "[")))))
(check-exn exn:fail? (lambda () (parse-expr (get-tokens (open-input-string "]")))))
(check-exn exn:fail? (lambda () (parse-expr (get-tokens (open-input-string "[[]")))))
(check-exn exn:fail? (lambda () (parse-expr (get-tokens (open-input-string "[[[][+[[[[]")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Now that we're satisfied with our own definitions, we provide them
;; for others to use us as a library.
(provide (all-defined-out))

