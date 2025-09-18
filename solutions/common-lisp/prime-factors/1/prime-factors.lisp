(defpackage :prime-factors
  (:use :cl)
  (:export :factors))

(in-package :prime-factors)

(defun check-factor (k n) 
  (let ((q 0) (r 0))
    (setf (values q r) (floor n k))
    (if (zerop r) q nil)))

 (defun factors (n)
  (loop :with curfac = 2 
        :with factors = nil   
        :with remv = nil
        :while (> n 1)
               do (setf remv (check-factor curfac n))
               do (if (null remv)
                   (incf curfac)
                   (progn
                     (setf factors (append factors (list curfac)))
                     (setf n remv)
                      ))             
        :finally (return factors)
                      ))
        
        
        
        