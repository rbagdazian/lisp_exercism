#lang racket

(provide sum-of-squares square-of-sum difference)

(define (sum-of-squares-acc number acc)
  (if (= number 0) acc
      (+ (sum-of-squares-acc (sub1 number) (* number number)) acc)))

(define (sum-of-squares number)
  (sum-of-squares-acc number 0))

(define (square-of-sum-acc number acc)
  (if (= number 0) acc
      (+ (square-of-sum-acc (sub1 number) number) acc)))
  

(define (square-of-sum number)
  (expt (square-of-sum-acc number 0) 2))

(define (difference number)
  (-  (square-of-sum number) (sum-of-squares number)))