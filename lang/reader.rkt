#lang racket/base


(require racket/list
         rackunit)


;; http://en.wikipedia.org/wiki/Brainfuck
;;
;; The language consists of programs using the following characters:
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
;; It ignores anything else.
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





;; A token stream represents the list of tokens we can get from the
;; system.
(define-struct tstream (elts) #:mutable)

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
(define (next a-tstream)
  (cond
   [(empty? (tstream-elts a-tstream))
    eof]
   [else
    (let ([next-token (first (tstream-elts a-tstream))])
      (set-tstream-elts! a-tstream (rest (tstream-elts a-tstream)))
      next-token)]))

;; peek: tstream -> (U token eof)
(define (peek a-tstream)
  (cond
   [(empty? (tstream-elts a-tstream))
    eof]
   [else
    (first (tstream-elts a-tstream))]))


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


  

;; ;; parse-expr: input-port -> syntax-object
;; (define (parse-exprs src in)
;;   (let ([tokens (get-tokens in)])
;;     (cond
;;      [(empty? tokens)
;;       (error 'parse-expr "unexpected end of tokens")]
;;      [else
;;       (case (first tokens)
;;         [(#\>)
;;          (datum->syntax #f '(increment-data-pointer))]
;;         [(#\<)
;;          (datum->syntax #f '(decrement-data-pointer))]
;;         [(#\+)
;;          (datum->syntax #f '(increment-byte))]
;;         [(#\-)
;;          (datum->syntax #f '(decrement-byte))]
;;         [(#\.)
;;          (datum->syntax #f '(output-byte))]
;;         [(#\,)
;;          (datum->syntax #f '(accept-byte))]
;;         [(#\[)
;;          (let ([inner-exprs
;;                 (parse-exprs (rest tokens))])
         







;; (define (my-read in)
;;   (syntax->datum
;;    (my-read-syntax #f in)))


;; (define (my-read-syntax src in)
;;   (void))


;; (provide (rename-out [my-read read]
;;                      [my-read-syntax read-syntax]))
