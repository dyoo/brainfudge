#lang setup/infotab
(define name "bf: a brainf*ck compiler for Racket")
(define categories '(devtools))
(define can-be-loaded-with 'all)
(define required-core-version "5.1.1")
(define version "1.7")
(define repositories '("4.x"))
(define scribblings '(("manual.scrbl" () (getting-started))))
(define primary-file "language.rkt")
(define blurb 
  '("bf: a brainf*ck compiler for Racket.  Includes a tutorial for building a language in Racket."))
(define release-notes
  '((p "More internal changes to make the generated code run faster.  The internal representation defines the state in two pieces, and also uses racket/unsafe/ops.")))