#lang setup/infotab
(define name "bf: a brainf*ck compiler for Racket")
(define categories '(devtools))
(define can-be-loaded-with 'all)
(define required-core-version "5.1.1")
(define version "1.2")
(define repositories '("4.x"))
(define scribblings '(("manual.scrbl")))
(define primary-file "language.rkt")
(define blurb 
  '("Provides support for the brainf*ck language."))
(define release-notes
  '((p "First release")))