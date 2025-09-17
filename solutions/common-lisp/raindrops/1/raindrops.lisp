(defpackage :raindrops
  (:use :cl)
  (:export :convert))

(in-package :raindrops)
              
(defun convert (number) 
  (let ((result "") (ok nil))
    (if (integerp (/ number 3)) (progn (setq result (concatenate 'string result "Pling")) (setq ok t)))
    (if (integerp (/ number 5)) (progn (setq result (concatenate 'string result "Plang")) (setq ok t)))
    (if (integerp (/ number 7)) (progn (setq result (concatenate 'string result "Plong")) (setq ok t)))
    (if (not ok) (concatenate 'string result (format nil "~A" number)) result)))



