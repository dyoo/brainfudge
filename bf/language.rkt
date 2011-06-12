#lang racket

(require "semantics.rkt")

(provide greater-than
         less-than
         plus
         minus
         period
         comma
         brackets
         #%module-begin)

(define *THE-STATE* (new-state))

(define-syntax-rule (greater-than)
  (increment-ptr *THE-STATE*))

(define-syntax-rule (less-than)
  (decrement-ptr *THE-STATE*))

(define-syntax-rule (plus)
  (increment-byte *THE-STATE*))

(define-syntax-rule (minus)
  (decrement-byte *THE-STATE*))

(define-syntax-rule (period)
  (write-byte-to-stdout *THE-STATE*))

(define-syntax-rule (comma)
  (read-byte-from-stdin *THE-STATE*))

(define-syntax-rule (brackets body ...)
  (loop *THE-STATE* body ...))


