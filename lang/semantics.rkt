#lang racket/base

;; http://en.wikipedia.org/wiki/Brainfuck
;;
;; This file defines the semantics of a Brainfudge program.
;;
;;
;; A program is expected to use the following forms below.
;; 
;; We use syntax rules here, but we could easily use regular functions
;; instead.  I'm tempted to do so.



(define DATA-SIZE 30000)
(define dataptr (box 0))
(define data (make-vector DATA-SIZE 0))


(define-syntax-rule (toplevel body ...)
  (begin
    (set-box! dataptr 0)
    (for ([i (in-range (vector-length data))])
         (vector-set! data i 0))
    body ...))


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



(provide toplevel
         increment-data-pointer
         decrement-data-pointer
         increment-byte
         decrement-byte
         output-byte
         accept-byte
         loop)
