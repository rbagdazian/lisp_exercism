(defpackage :sieve
  (:use :cl)
  (:export :primes-to)
  (:documentation "Generates a list of primes up to a given limit."))

(in-package :sieve)

(defun filt (seq n)
    (do ((curv (* n 2) (+ curv n))
         (lastv (car (last seq)) lastv))
        ((> curv lastv) seq)
        (delete curv seq) )) 
        
      
        


(defun primes-to (n)
  "List primes up to `n' using sieve of Eratosthenes."
  (let ((acc (loop for i from 1 to n collect i)) 
        (maxv (floor (/ n 2))))
  (cond ((< n 2) nil)
        ((= n 2) (list 2))
        (t (progn (loop for idx from 2 to maxv 
              do (filt acc idx)
              )
             (delete 1 acc)
        )))))

