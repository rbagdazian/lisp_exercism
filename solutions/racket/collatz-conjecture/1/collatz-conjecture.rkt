#lang racket

(provide collatz)
  (require rackunit rackunit/text-ui)

(define (collatz num)
  [let ( [ct 0])
    (do
        ([cv num] )
      ([= cv 1] ct)
      (cond
        ([not (integer? cv)] (error "Integers only as input"))
        ([<= cv 0] (error "Initial input must be a positive integer"))
        ([= cv 1] ct)
        ([= (remainder cv 2) 0]
         (set! cv (quotient cv 2))
         (set! ct (add1 ct)))
        (#t
         (set! cv (+ (* 3 cv) 1))
         (set! ct (add1 ct)))
        )
      )
    ])
