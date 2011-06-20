#lang racket

;; This is a second semantics for the language that tries to go for speed.
;; It uses features in: http://docs.racket-lang.org/reference/unsafe.html
;; to reduce the number of runtime checks.

(require rackunit               ;; we want unit tests
         racket/unsafe/ops      ;; and we want raw, unsafe access for speed
         )


(provide (all-defined-out))


;; Our state includes two pieces.
(define-struct state (data ptr)
  #:mutable)

;; Creates a new state, with a byte array of 30000 zeros, and
;; the pointer at index 0.
(define (new-state) 
  (make-state (make-vector 30000 0)
              0))

;; state-ptr-out-of-range?: state -> boolean
;; Produces true if the pointer went off the data array.
(define-syntax-rule (state-ptr-out-of-range? a-state)
  (or (unsafe-fx>= (state-ptr a-state)
          (unsafe-vector-length (state-data a-state)))
      (unsafe-fx< (state-ptr a-state) 0)))


;; Check to see if we've gone out of range.  If we have a useful stx
;; to blame, use that syntax to highlight on screen.
(define-syntax-rule (detect-range-errors! a-state caller-name stx)
  (when (state-ptr-out-of-range? a-state)
    (if stx
        (raise-syntax-error #f "pointer went out of range of data" stx)
        (error caller-name "pointer went out of range of data"))))


;; increment the data pointer
(define-syntax-rule (increment-ptr a-state stx)
  (begin
    (set-state-ptr! a-state (unsafe-fx+ (state-ptr a-state) 1))
    (detect-range-errors! a-state 'increment-ptr stx)))

;; decrement the data pointer
(define-syntax-rule (decrement-ptr a-state stx)
  (begin
    (set-state-ptr! a-state (unsafe-fx- (state-ptr a-state) 1))
    (detect-range-errors! a-state 'decrement-ptr stx)))

;; increment the byte at the data pointer
(define-syntax-rule (increment-byte a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (unsafe-vector-set! v i (unsafe-fx+ (unsafe-vector-ref v i) 1))))

;; decrement the byte at the data pointer
(define-syntax-rule (decrement-byte a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (unsafe-vector-set! v i (unsafe-fx- (unsafe-vector-ref v i) 1))))

;; print the byte at the data pointer
(define-syntax-rule (write-byte-to-stdout a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (write-byte (unsafe-vector-ref v i) (current-output-port))))

;; read a byte from stdin into the data pointer
(define-syntax-rule (read-byte-from-stdin a-state)
  (let ([v (state-data a-state)]
        [i (state-ptr a-state)])
    (unsafe-vector-set! v i (let ([v (read-byte (current-input-port))])
                              (if (eof-object? v)
                                  0
                                  v)))))

;; we know how to do loops!
(define-syntax-rule (loop a-state body ...)
  (let loop ()
    (unless (= (unsafe-vector-ref (state-data a-state)
                                  (state-ptr a-state))
               0)
      body ...
      (loop))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some tests follow:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Simple exercises.
(let ([s (new-state)])
  (increment-byte s)
  (check-equal? 1 (vector-ref (state-data s) 0))
  (increment-byte s)
  (check-equal? 2 (vector-ref (state-data s) 0))
  (decrement-byte s)
  (check-equal? 1 (vector-ref (state-data s) 0)))

;; pointer movement
(let ([s (new-state)])
  (increment-ptr s #f)
  (increment-byte s)
  (check-equal? 0 (vector-ref (state-data s) 0))
  (check-equal? 1 (vector-ref (state-data s) 1))
  (decrement-ptr s #f)
  (increment-byte s)
  (check-equal? 1 (vector-ref (state-data s) 0))
  (check-equal? 1 (vector-ref (state-data s) 1)))

;; make sure standard input is doing something
(let ([s (new-state)])
  (parameterize ([current-input-port
                  (open-input-bytes (bytes 3 1 4))])
    (read-byte-from-stdin s)
    (increment-ptr s #f)
    (read-byte-from-stdin s)
    (increment-ptr s #f)
    (read-byte-from-stdin s))
  (check-equal? 3 (vector-ref (state-data s) 0))
  (check-equal? 1 (vector-ref (state-data s) 1))
  (check-equal? 4 (vector-ref (state-data s) 2)))


;; make sure standard output is doing something
(let ([s (new-state)])
  (set-state-data! s (vector 80 76 84))
  (let ([simulated-stdout (open-output-string)])
    (parameterize ([current-output-port simulated-stdout])
      (write-byte-to-stdout s)
      (increment-ptr s #f)
      (write-byte-to-stdout s)
      (increment-ptr s #f)
      (write-byte-to-stdout s))
    (check-equal? "PLT" (get-output-string simulated-stdout))))


;; Let's see that we can clear.
(let ([s (new-state)])
  (set-state-data! s (vector 0 104 101 108 112 109 101 105
                             109 109 101 108 116 105 110 103 ))
  (set-state-ptr! s 15)
  ;; [ [-] < ]
  (loop s 
        (loop s (decrement-byte s))
        (decrement-ptr s #f))
  
  (check-equal? 0 (state-ptr s))
  (check-equal? (make-vector 16 0) (state-data s)))
                 