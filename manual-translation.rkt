#lang racket/base
(require "lang/brainfudge.rkt")

#|

+++++ +++++             initialize counter (cell #0) to 10
[                       use loop to set the next four cells to 70/100/30/10
    > +++++ ++              add  7 to cell #1
    > +++++ +++++           add 10 to cell #2 
    > +++                   add  3 to cell #3
    > +                     add  1 to cell #4
    <<<< -                  decrement counter (cell #0)
]

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


(let/ec escape
  (let loop ()
    (parameterize ([current-escape escape])
      (conditional-escape)
      
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
      (loop))))
