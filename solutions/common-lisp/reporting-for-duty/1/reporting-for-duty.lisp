(defpackage :reporting-for-duty
  (:use :cl)
  (:export :format-quarter-value :format-two-quarters
           :format-two-quarters-for-reading))

(in-package :reporting-for-duty)

;; Define format-quarter-value function.
(defun format-quarter-value (quarter value)
  (format nil "The value ~a quarter: ~a" quarter value))

;; Define format-two-quarters function.
(defun format-two-quarters (stream str1 value1 str2 value2)
  (format stream "~%~A~%~A~%" (format-quarter-value str1 value1) (format-quarter-value str2 value2)))

;; Define format-two-quarters-for-reading function.

(defun format-two-quarters-for-reading (stream str1 value1 str2 value2)
   (format stream "(\"The value ~a quarter: ~a\" \"The value ~a quarter: ~a\")" str1 value1 str2 value2))
