(defpackage :proverb
  (:use :cl)
  (:export :recite))

(in-package :proverb)

(defun recite (strings &aux (result ""))
  (loop for i from 0 to (1- (length strings)) do        
        (if (= i (1- (length strings))) 
            (setf result (concatenate 'string result (format nil "And all for the want of a ~A." (nth 0 strings))))
            (setf result (concatenate 'string result (format nil "For want of a ~A the ~A was lost.~%" (nth i strings) (nth (1+ i) strings))))
        )
        )
  result
      )
