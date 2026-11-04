#lang racket

(provide score)

(define (score x y)
  (let ([rad (sqrt (+ (* x x) (* y y)))])
  (if (<= rad 1) 10
      (if (<= rad 5) 5
          (if (<= rad 10) 1
              0)))
    )
  )
