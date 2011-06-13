#lang racket

(require "semantics.rkt")

(provide greater-than
         less-than
         plus
         minus
         period
         comma
         brackets
         (rename-out [my-module-begin #%module-begin]))

;; The current-state is a parameter used by
;; the rest of the language.
(define current-state (make-parameter (new-state)))

;; Every module in this language will make sure that it
;; uses a fresh state.
(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
   (parameterize ([current-state (new-state)])
     body ...)))

(define-syntax-rule (greater-than)
  (increment-ptr (current-state)))

(define-syntax-rule (less-than)
  (decrement-ptr (current-state)))

(define-syntax-rule (plus)
  (increment-byte (current-state)))

(define-syntax-rule (minus)
  (decrement-byte (current-state)))

(define-syntax-rule (period)
  (write-byte-to-stdout (current-state)))

(define-syntax-rule (comma)
  (read-byte-from-stdin (current-state)))

(define-syntax-rule (brackets body ...)
  (loop (current-state) body ...))


