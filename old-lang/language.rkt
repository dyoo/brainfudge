#lang racket/base

;;
;; This takes the semantics and presents it as a "module language",
;; in the sense described in:
;;
;;    http://docs.racket-lang.org/guide/module-languages.html
;;


(require "semantics.rkt")
(provide (all-from-out "semantics.rkt")
         (rename-out [#%plain-module-begin #%module-begin]))
