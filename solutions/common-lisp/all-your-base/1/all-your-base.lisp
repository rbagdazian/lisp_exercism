(defpackage :all-your-base
  (:use :cl)
  (:export :rebase))

(in-package :all-your-base)

(defun from-base (list-in base pos)
   (cond ((null list-in) 0)
           (T 
                (+ (* (car list-in) (expt base pos)) 
                   (from-base (cdr list-in) base (+ pos 1))))))

(defun to-base (val base pos)
    (cond ((zerop val) '(0))
      (T (append (to-base (- val (mod val (expt base (+ pos 1)))) base (+ pos 1))
                      (list (/ (mod val (expt base (+ pos 1))) (expt base pos)))))))

(defun ztrim (aList) 
  (cond ((null (cdr aList)) aList) ; return single item list
        ((zerop (car aList)) (ztrim (cdr aList))) ;remove leading zero, and call with rest of list
        (T aList))) ; return list if no leading zero present

(defun allvalid (aList base)
  (cond ((null aList) T) ; return true if we reached the end of the list
        ((>= (car aList) base) nil) ; error if digit is >= the base.
        ; make sure leading digit >= 0 then call recursively on rest of list.
        ((>= (car aList) 0) (allvalid (cdr aList) base))))
                            
(defun rebase (list-digits in-base out-base)
  (cond ((<= in-base 1) nil) ; check for legitimate base on input
        ((<= out-base 1) nil) ; check for legitimate base on output
        ((null list-digits) '(0)) ; check for null list of input digits
        ((not (allvalid list-digits in-base)) nil) ; check to make sure all digits in valid range
        ; now trim of leading zeros, then perform transform.
        (T (ztrim (to-base (from-base (reverse list-digits) in-base 0) out-base 0))))) 



