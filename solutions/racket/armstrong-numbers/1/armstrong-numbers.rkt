#lang racket/base

(provide armstrong-number?)

(define (get-digits n acc)
  (cond ((= n 0) (reverse acc))
        (#t (get-digits (quotient n 10) (append acc (list (remainder n 10)))))
        ))


(define (armstrong-number? n)
  (let* ((digits (get-digits n '()))
         (dlen (length digits))
         (sumv (apply + (map (λ (x) (expt x dlen)) digits))) )
    (= sumv n)
    ))


