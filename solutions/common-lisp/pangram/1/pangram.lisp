(defpackage :pangram
  (:use :cl)
  (:export :pangramp))

(in-package :pangram)

(defun pangramp (sentence &aux alist)
  (loop for ch across (string-downcase sentence) do
        (if (and (lower-case-p ch) (not (assoc ch alist)))
            (setf alist (push (cons ch 1) alist))))
  (= (length alist) 26)
  )
