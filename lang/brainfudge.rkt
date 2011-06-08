#lang racket/base

;; http://en.wikipedia.org/wiki/Brainfuck

(define dataptr 0)
(define MAX-DATA-INDEX 29999)
(define data (make-vector (add1 MAX-DATA-INDEX) 0))

(define current-escape (make-parameter (lambda () (void))))


;; >
(define (increment-data-pointer)
  (set! dataptr (min (add1 dataptr)
                     MAX-DATA-INDEX)))


;; <
(define (decrement-data-pointer)
  (set! dataptr (max (sub1 dataptr) 0)))


;; +
(define (increment-byte)
  (vector-set! data dataptr
               ;; What happens at overflow?
               (add1 (vector-ref data dataptr))))



;; -
(define (decrement-byte)
  (vector-set! data dataptr
               ;; What happens at underflow?
               (sub1 (vector-ref data dataptr))))


;; .
(define (output-byte)
  (write-byte (vector-ref data dataptr)))


;; ,
(define (accept-byte)
  (vector-set! data dataptr (read-byte)))


;; [
(define (conditional-escape)
  (when (= (vector-ref data dataptr) 0)
    ((current-escape))))




(provide increment-data-pointer
         decrement-data-pointer
         increment-byte
         decrement-byte
         output-byte
         accept-byte
         conditional-escape

         current-escape)
