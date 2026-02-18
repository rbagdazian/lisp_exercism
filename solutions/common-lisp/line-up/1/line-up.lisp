(defpackage :line-up
  (:use :cl)
  (:shadow format)
  (:export :format))

(in-package :line-up)

(defun format (name number)
  
  (let ((last-digit (mod number 10))
        (last-two (mod number 100))
        (suffix "")
        (res "")
        )
       (cond 
         ((and (= last-digit 1) (/= last-two 11)) (setf suffix "st"))
         ((and (= last-digit 2) (/= last-two 12)) (setf suffix "nd"))
         ((and (= last-digit 3) (/= last-two 13)) (setf suffix "rd"))
         (t (setf suffix "th"))
         )
       (setf res (cl:format nil "~a, you are the ~d~a customer we serve today. Thank you!" name number suffix))
       res
       ))
