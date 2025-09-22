(defpackage :hamming
  (:use :cl)
  (:export :distance))

(in-package :hamming)

(defun gethamming (s1 s2 acc)
  (if (= (length s1) 0)
      acc
      (if (eql (char s1 0) (char s2 0))
          (gethamming (subseq s1 1) (subseq s2 1) acc)
          (gethamming (subseq s1 1) (subseq s2 1) (1+ acc) ))))

(defun distance (strand1 strand2)
  (let ((acc 0) (lt 0))
       (if (not (eql (length strand1) (length strand2)))
           nil
           ;otherwise
           (gethamming strand1 strand2 0)
           )))

