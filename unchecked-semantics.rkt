#lang racket

;; This is a second semantics for the language that tries to go for speed,
;; at the expense of making things a little more complicated.
;;
;; It uses features in: http://docs.racket-lang.org/reference/unsafe.html
;; to reduce the number of runtime checks.
;;
;; We also manage the state as two separate values.

(require rackunit               ;; we want unit tests
         racket/unsafe/ops)      ;; and we want raw, unsafe access for speed


(provide (all-defined-out))


(define-syntax MAX-DATA-SIZE
  (lambda (stx) #'30000))


;; We use a customized error structure that supports
;; source location reporting.
(define-struct (exn:fail:out-of-bounds exn:fail)
  (srcloc)
  #:property prop:exn:srclocs
             (lambda (a-struct)
               (list (exn:fail:out-of-bounds-srcloc a-struct))))



;; Creates a new state, with a byte array of 30000 zeros, and
;; the pointer at index 0.
(define (new-state) 
  (values (make-bytes MAX-DATA-SIZE 0)
          0))


;; Check to see if we've gone out of range.  If we have a useful stx
;; to blame, use that syntax to highlight on screen.
(define-syntax-rule (raise-range-errors! a-state caller-name loc)
  (raise (make-exn:fail:out-of-bounds
          (format "~a: pointer went out of range of data"
                  caller-name)
          (current-continuation-marks)
          (apply srcloc loc))))


;; increment the data pointer
(define-syntax-rule (increment-ptr data ptr loc)
  (begin
    (set! ptr (unsafe-fx+ ptr 1))
    (when (unsafe-fx>= ptr MAX-DATA-SIZE)
      (raise-range-errors! a-state 'increment-ptr loc))))


;; decrement the data pointer
(define-syntax-rule (decrement-ptr data ptr loc)
  (begin
    (set! ptr (unsafe-fx- ptr 1))
    (when (unsafe-fx< ptr 0)
      (raise-range-errors! a-state 'decrement-ptr loc))))


;; increment the byte at the data pointer
(define-syntax-rule (increment-byte data ptr)
  (unsafe-bytes-set! data ptr (unsafe-fxmodulo (unsafe-fx+ (unsafe-bytes-ref data ptr) 1) 256)))

;; decrement the byte at the data pointer
(define-syntax-rule (decrement-byte data ptr)
  (unsafe-bytes-set! data ptr (unsafe-fxmodulo (unsafe-fx- (unsafe-bytes-ref data ptr) 1) 256)))

;; print the byte at the data pointer
(define-syntax-rule (write-byte-to-stdout data ptr)
  (write-byte (unsafe-bytes-ref data ptr) (current-output-port)))

;; read a byte from stdin into the data pointer
(define-syntax-rule (read-byte-from-stdin data ptr)
  (unsafe-bytes-set! data ptr (let ([v (read-byte (current-input-port))])
                                 (if (eof-object? v)
                                     0
                                     v))))

;; Loops
(define-syntax-rule (loop data ptr body ...)
  (unless (unsafe-fx= (unsafe-bytes-ref data ptr)
                      0)
    (let loop ()
      body ...
      (unless (unsafe-fx= (unsafe-bytes-ref data ptr)
                          0)
        (loop)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some tests follow:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Simple exercises.
(let-values ([(data ptr) (new-state)])
  (increment-byte data ptr)
  (check-equal? 1 (bytes-ref data 0))
  (increment-byte data ptr)
  (check-equal? 2 (bytes-ref data 0))
  (decrement-byte data ptr)
  (check-equal? 1 (bytes-ref data 0)))

;; pointer movement
(let-values ([(data ptr) (new-state)])
  (increment-ptr data ptr #f)
  (increment-byte data ptr)
  (check-equal? 0 (bytes-ref data 0))
  (check-equal? 1 (bytes-ref data 1))
  (decrement-ptr data ptr #f)
  (increment-byte data ptr)
  (check-equal? 1 (bytes-ref data 0))
  (check-equal? 1 (bytes-ref data 1)))

;; make sure standard input is doing something
(let-values ([(data ptr) (new-state)])
  (parameterize ([current-input-port
                  (open-input-bytes (bytes 3 1 4))])
    (read-byte-from-stdin data ptr)
    (increment-ptr data ptr #f)
    (read-byte-from-stdin data ptr)
    (increment-ptr data ptr #f)
    (read-byte-from-stdin data ptr))
  (check-equal? 3 (bytes-ref data 0))
  (check-equal? 1 (bytes-ref data 1))
  (check-equal? 4 (bytes-ref data 2)))


;; make sure standard output is doing something
(let-values ([(data ptr) (new-state)])
  (set! data (bytes 80 76 84))
  (let ([simulated-stdout (open-output-string)])
    (parameterize ([current-output-port simulated-stdout])
      (write-byte-to-stdout data ptr)
      (increment-ptr data ptr #f)
      (write-byte-to-stdout data ptr)
      (increment-ptr data ptr #f)
      (write-byte-to-stdout data ptr))
    (check-equal? "PLT" (get-output-string simulated-stdout))))


;; Let's see that we can clear.
(let-values ([(data ptr) (new-state)])
  (set! data (bytes 0 104 101 108 112 109 101 105
                     109 109 101 108 116 105 110 103 ))
  (set! ptr 15)
  ;; [ [-] < ]
  (loop data ptr 
        (loop data ptr (decrement-byte data ptr))
        (decrement-ptr data ptr #f))
  
  (check-equal? 0 ptr)
  (check-equal? (make-bytes 16 0) data))
