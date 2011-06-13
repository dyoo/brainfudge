#lang s-exp syntax/module-reader
(planet dyoo/brainfudge/lang/language)
#:read my-read
#:read-syntax my-read-syntax


;; Now that we have a parser, let't provide the two functions that a
;; language reader module needs to define.  These are the read and
;; read-syntax functions.
(require "parser.rkt")



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

  ;; We also need to return eof once we've exhausted the token stream,
  ;; so let's do so.
  (let ([token-stream (get-tokens in)])
    (cond
     [(eof-object? (peek token-stream))
      eof]
     [else
      (parse-toplevel token-stream)])))