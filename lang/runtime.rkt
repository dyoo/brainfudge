#lang racket/base

;; http://en.wikipedia.org/wiki/Brainfuck

(define dataptr (box 0))
(define DATA-SIZE 30000)
(define data (make-vector DATA-SIZE 0))


;; Look at the byte at the data pointer.
(define-syntax-rule (ref)
  (vector-ref data (unbox dataptr)))


;; >
(define-syntax-rule (increment-data-pointer)
  (set-box! dataptr (modulo (add1 (unbox dataptr)) DATA-SIZE)))


;; <
(define-syntax-rule (decrement-data-pointer)
  (set-box! dataptr (modulo (sub1 (unbox dataptr)) DATA-SIZE)))


;; +
(define-syntax-rule (increment-byte)
  (vector-set! data (unbox dataptr)
               (modulo (add1 (ref))
                       256)))

;; -
(define-syntax-rule (decrement-byte)
  (vector-set! data (unbox dataptr)
               (modulo (sub1 (ref))
                       256)))


;; .
(define-syntax-rule (output-byte)
  (write-byte (ref)))


;; ,
(define-syntax-rule (accept-byte)
  (vector-set! data (unbox dataptr) (read-byte)))


;; [
;; Call an-escape if the data under the pointer is zero.
(define-syntax-rule (conditional-escape an-escape)
  (when (= (vector-ref data (unbox dataptr)) 0)
    (an-escape)))


(provide increment-data-pointer
         decrement-data-pointer
         increment-byte
         decrement-byte
         output-byte
         accept-byte
         conditional-escape)
