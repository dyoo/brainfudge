#lang racket/base

(require "semantics.rkt"
         racket/stxparam
         (for-syntax racket/base))

(provide greater-than
         less-than
         plus
         minus
         period
         comma
         brackets
         (rename-out [my-module-begin #%module-begin]))



;; We define a syntax parameter called current-state here.
;; This cooperates with the other forms in this language.  See
;; my-module-begin's comments for more details.
(define-syntax-parameter current-data #f)
(define-syntax-parameter current-ptr #f)




;; Every module in this language will make sure that it
;; uses a fresh state.  We create one, and then within
;; the lexical context of a my-module-begin, all the
;; other forms will refer to current-state.
(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (syntax/loc stx
       (#%plain-module-begin
        
        (define (run)
          (let-values ([(fresh-state fresh-ptr) (new-state)])
            
            ;; Here are the mechanics we're using to get all the other
            ;; forms to use this fresh state.
            ;;
            ;; We use the syntax parameter library to make
            ;; any references to current-state within the body to
            ;; syntactically re-route to the fresh-state we create here.
            (syntax-parameterize ([current-data
                                   (make-rename-transformer #'fresh-state)]
                                  [current-ptr
                                   (make-rename-transformer #'fresh-ptr)])
               (begin body ... (void)))))
        (run)))]))


;; In order to produce good runtime error messages
;; for greater-than and less-than, we latch onto 
;; the syntax object for dear life, since it has
;; information about where it came from in the
;; source syntax.
;;
;; The #'#,stx nonsense below allows us to pass the
;; syntax object.  The semantics can then raise an
;; appropriate syntactic error with raise-syntax-error
;; if it sees anything bad happen at runtime.
(define-syntax (greater-than stx)
  (syntax-case stx ()
    [(_)
     (quasisyntax/loc stx
       (increment-ptr current-data current-ptr
                      (srcloc '#,(syntax-source stx)
                              '#,(syntax-line stx)
                              '#,(syntax-column stx)
                              '#,(syntax-position stx)
                              '#,(syntax-span stx))))]))


(define-syntax (less-than stx)
  (syntax-case stx ()
    [(_)
     (quasisyntax/loc stx
       (decrement-ptr current-data current-ptr
                      (srcloc '#,(syntax-source stx)
                              '#,(syntax-line stx)
                              '#,(syntax-column stx)
                              '#,(syntax-position stx)
                              '#,(syntax-span stx))))]))


(define-syntax-rule (plus)
  (increment-byte current-data current-ptr))

(define-syntax-rule (minus)
  (decrement-byte current-data current-ptr))

(define-syntax-rule (period)
  (write-byte-to-stdout current-data current-ptr))

(define-syntax-rule (comma)
  (read-byte-from-stdin current-data current-ptr))

(define-syntax-rule (brackets body ...)
  (loop current-data current-ptr body ...))