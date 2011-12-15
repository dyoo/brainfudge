#lang racket/base

(provide inject-javascript inject-css)

(require scribble/core
         scribble/html-properties
         scriblib/render-cond)

;; Adds JavaScript if we're rendering in HTML.
(define (inject-javascript . body)
  (cond-element 
   [latex ""]
   [html (make-element (make-style #f (list (make-script-property "text/javascript"
                                                           body)))
                       '())]
   [text ""]))

(define (inject-css path)
  (cond-element 
   [latex ""]
   [html (make-element (make-style #f (list (make-css-addition path)))
                       '())]
   [text ""]))



;;(define (google-analytics)
;;  (make-tag 