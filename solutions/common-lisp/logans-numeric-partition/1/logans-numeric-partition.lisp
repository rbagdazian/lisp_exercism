(defpackage :logans-numeric-partition
  (:use :cl)
  (:export :categorize-number :partition-numbers))

(in-package :logans-numeric-partition)

;; Define categorize-number function
(defun categorize-number (pair val)
  (if (oddp val)
      (cons (setf (car pair) (append (list val) (car pair))) (cdr pair))
      (cons (car pair) (setf (cdr pair) (append (list val) (cdr pair))))
      )
  )

;; Define partition-numbers function
 (defun partition-numbers (seq)
  (let ((part (cons nil nil)))
       (setf part (reduce #'categorize-number seq :initial-value part))))
