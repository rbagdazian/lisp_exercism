(defpackage :roman-numerals
  (:use :cl)
  (:export :romanize))

(in-package :roman-numerals)


(defvar *roman-numbers*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash 1 ht) "I")
    (setf (gethash 4 ht) "IV")
    (setf (gethash 5 ht) "V")
    (setf (gethash 9 ht) "IX")
    (setf (gethash 10 ht) "X")
    (setf (gethash 40 ht) "XL")
    (setf (gethash 50 ht) "L")
    (setf (gethash 90 ht) "XC")
    (setf (gethash 100 ht) "C")
    (setf (gethash 400 ht) "CD")
    (setf (gethash 500 ht) "D")
    (setf (gethash 900 ht) "CM")
    (setf (gethash 1000 ht) "M")
    ht))

(defvar *roman-values* '(1000 900 500 400 100 90 50 40 10 9 5 4 1))

(defun romanize (num)
  "Convert a positive integer to Roman numerals."
  (when (< num 1)
    (error "Roman numerals cannot represent numbers less than 1"))
  (let ((result ""))
    (dolist (value *roman-values* result)
      (loop while (>= num value) do
        (setf result (concatenate 'string result (gethash value *roman-numbers*)))
        (decf num value)))))
