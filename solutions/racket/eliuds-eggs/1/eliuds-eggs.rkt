#lang racket

(provide number->eggs)

(define (number->eggs n)
  (let ([d (bitwise-and n 1)])
  (if (= n 0) 0  
      (+ (number->eggs (quotient n 2) ) d))
  ))


