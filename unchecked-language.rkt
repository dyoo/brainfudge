#lang racket

(require "semantics.rkt"
         racket/stxparam)

(provide greater-than
         less-than
         plus
         minus
         period
         comma
         brackets
         (rename-out [my-module-begin #%module-begin]))


(define-syntax-parameter current-state #f)


;; Every module in this language will make sure that it
;; uses a fresh state.  We create one, and then within
;; the lexical context of a my-module-begin, all the
;; other forms will refer to current-state.
(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (with-syntax ([s 's])
       (syntax/loc stx
         (#%plain-module-begin
          (let ([s (new-state)])
            (syntax-parameterize ([current-state (make-rename-transformer #'s)])
               (begin body ... (void)))))))]))


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
       (increment-ptr current-state #'#,stx))]))

(define-syntax (less-than stx)
  (syntax-case stx ()
    [(_)
     (quasisyntax/loc stx
       (decrement-ptr current-state #'#,stx))]))

(define-syntax-rule (plus)
  (increment-byte current-state))

(define-syntax-rule (minus)
  (decrement-byte current-state))

(define-syntax-rule (period)
  (write-byte-to-stdout current-state))

(define-syntax-rule (comma)
  (read-byte-from-stdin current-state))

(define-syntax-rule (brackets body ...)
  (loop current-state body ...))