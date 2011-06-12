#lang s-exp syntax/module-reader
racket
#:read my-read
#:read-syntax my-read-syntax

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (let ([next-line (read-line in)])
    (cond
     [(eof-object? next-line)
      eof]
     [else
      (datum->syntax #f next-line)])))