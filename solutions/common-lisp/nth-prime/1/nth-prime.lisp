(defpackage :nth-prime
  (:use :cl)
  (:export :find-prime))

(in-package :nth-prime)

(defun eliminate-multiples (list n)
  "Removes all multiples of n from a given list. 
  and return the modified list, n is moved to the end of the list
  before returning"
  (append
      (remove-if (lambda (x) (zerop (mod x n))) list)
      (list n)))      

(defun primes-to (n)
  "List primes up to n using sieve of Eratosthenes."
  ; initialize the array with integer set
  (let ((acc (loop for i from 2 to n collect i)) 
        (maxv (floor (sqrt n))))
  (cond ((< n 2) nil)
        ((= n 2) (list 2))
        (t (progn
             (do ((idx 2 (car acc)))
                ((> idx maxv) acc)
                (setf acc (eliminate-multiples acc idx))               
               )
             (setf acc (sort acc #'<)))
           ))))
              
 (defun find-prime (n)
  (if (zerop n) nil
      (loop :with primes-tov
        :with curlim = 110000
        :with nthpr = nil 
        :while (null nthpr)
            :do (setf primes-tov (remove 0 (primes-to curlim)))
            :do (if (null (nth n primes-tov)) 
                    (setf curlim (* curlim 2))
                    (setf nthpr (nth (1- n) primes-tov)))
        :finally (return nthpr))
   ))



