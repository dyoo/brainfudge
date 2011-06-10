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


;; [ ... ]
(define-syntax-rule (loop body ...)
  (let repeat ()
    (if (= (vector-ref data (unbox dataptr)) 0)
        (void)
        (begin
          body ...
          (repeat)))))



(provide increment-data-pointer
         decrement-data-pointer
         increment-byte
         decrement-byte
         output-byte
         accept-byte
         loop)
