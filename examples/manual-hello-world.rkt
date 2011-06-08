#lang racket/base
(require "../lang/runtime.rkt")

#|

A manual translation of the hello world program, just to make sure I
have the right understanding of the evaluation.



+++++ +++++             initialize counter (cell #0) to 10
[                       use loop to set the next four cells to 70/100/30/10
    > +++++ ++              add  7 to cell #1
    > +++++ +++++           add 10 to cell #2 
    > +++                   add  3 to cell #3
    > +                     add  1 to cell #4
    <<<< -                  decrement counter (cell #0)
]


> ++ .                  print 'H'
> + .                   print 'e'
+++++ ++ .              print 'l'
.                       print 'l'
+++ .                   print 'o'
> ++ .                  print ' '
<< +++++ +++++ +++++ .  print 'W'
> .                     print 'o'
+++ .                   print 'r'
----- - .               print 'l'
----- --- .             print 'd'
> + .                   print '!'
> .                     print '\n


|#


(increment-byte)
(increment-byte)
(increment-byte)
(increment-byte)
(increment-byte)
(increment-byte)
(increment-byte)
(increment-byte)
(increment-byte)
(increment-byte)



;; This is a translation of the loop.
(let/ec escape
  (let loop ()
    (conditional-escape escape)
    
    (increment-data-pointer)
    (increment-byte)
    (increment-byte)
    (increment-byte)
    (increment-byte)
    (increment-byte)

    (increment-byte)
    (increment-byte)

    (increment-data-pointer)
    (increment-byte)
    (increment-byte)
    (increment-byte)
    (increment-byte)
    (increment-byte)

    (increment-byte)
    (increment-byte)
    (increment-byte)
    (increment-byte)
    (increment-byte)

    (increment-data-pointer)
    (increment-byte)
    (increment-byte)
    (increment-byte)

    (increment-data-pointer)
    (increment-byte)

    (decrement-data-pointer)
    (decrement-data-pointer)
    (decrement-data-pointer)
    (decrement-data-pointer)

    (decrement-byte)
    (loop)))


;; > ++ .                  print 'H'
(increment-data-pointer) (increment-byte) (increment-byte) (output-byte)

;; > + .                   print 'e'
(increment-data-pointer) (increment-byte) (output-byte)

;; +++++ ++ .              print 'l'
(increment-byte)(increment-byte)(increment-byte)(increment-byte)(increment-byte) (increment-byte)(increment-byte) (output-byte)

;; .                       print 'l'
(output-byte)


;; +++ .                   print 'o'
(increment-byte) (increment-byte) (increment-byte) (output-byte)

;; > ++ .                  print ' '
(increment-data-pointer) (increment-byte) (increment-byte) (output-byte)


;; << +++++ +++++ +++++ .  print 'W'
(decrement-data-pointer) (decrement-data-pointer)
(increment-byte) (increment-byte)(increment-byte)(increment-byte)(increment-byte)
(increment-byte) (increment-byte)(increment-byte)(increment-byte)(increment-byte)
(increment-byte) (increment-byte)(increment-byte)(increment-byte)(increment-byte)
(output-byte)

;; > .                     print 'o'
(increment-data-pointer) (output-byte)

;; +++ .                   print 'r'
(increment-byte) (increment-byte) (increment-byte) (output-byte)

;; ----- - .               print 'l'
(decrement-byte) (decrement-byte) (decrement-byte) (decrement-byte) (decrement-byte)
(decrement-byte) (output-byte)

;; ----- --- .             print 'd'
(decrement-byte) (decrement-byte) (decrement-byte) (decrement-byte) (decrement-byte)
(decrement-byte) (decrement-byte) (decrement-byte) (output-byte)


;; > + .                   print '!'
(increment-data-pointer) (increment-byte) (output-byte)

;; > .                     print '\n
(increment-data-pointer) (output-byte)