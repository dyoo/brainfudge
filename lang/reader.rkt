#lang s-exp syntax/module-reader
(planet dyoo/bf/unchecked-language)   ;; switched from (planet dyoo/bf/language)
#:read my-read
#:read-syntax my-read-syntax

(require "../parser.rkt")

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (parse-expr src in))
