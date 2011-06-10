#lang racket/base

;; http://en.wikipedia.org/wiki/Brainfuck
;;
;; Just watch for the following characters:
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
;; and ignore anything else.
;;
;;
;; This module produces an AST
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



(define (my-read in)
  (syntax->datum
   (my-read-syntax #f in)))


(define (my-read-syntax src in)
  (void))