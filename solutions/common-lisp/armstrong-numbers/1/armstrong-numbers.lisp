(defpackage :armstrong-numbers
  (:use :cl)
  (:export :armstrong-number-p))
(in-package :armstrong-numbers)

 (defun get-seq (num acc)
  (cond ((= num 0) acc)
        (t (multiple-value-bind (q r) (floor num 10)
             (setf acc (nconc acc (list r)))
             (get-seq q acc)))))

(defun armstrong-number-p (number)
  (let* ((numseq (get-seq number '()))
         (expv (length numseq))
         (sumres (reduce #'(lambda (s x) (+ (expt x expv) s))  numseq :initial-value 0)))
       (= number sumres)))

  

