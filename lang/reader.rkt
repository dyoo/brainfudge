#lang s-exp syntax/module-reader
(planet dyoo/brainfudge/lang/lang)
#:read my-read
#:read-syntax my-read-syntax


(require "parser.rkt")

;; Now that we have a parser, let't provide the two functions that a
;; language reader module needs to define.

;; my-read: input-port -> s-expression
(define (my-read in)
  (syntax->datum
   (my-read-syntax #f in)))


;; my-read-syntax: any input-port -> syntax-object
;;
(define (my-read-syntax src in)
  ;; Note: we're ignoring our src, but if our parser were a bit nicer,
  ;; it would also keep track of the src in the resulting syntax
  ;; objects.
  (let ([token-stream (get-tokens in)])
    (cond
     [(eof-object? (peek token-stream))
      eof]
     [else
      (parse-toplevel token-stream)])))
