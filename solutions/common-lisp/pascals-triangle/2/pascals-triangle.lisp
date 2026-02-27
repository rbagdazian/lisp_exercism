(defpackage :pascals-triangle
  (:use :cl)
  (:export :rows))

(in-package :pascals-triangle)

 (defun get-new-row (prow crow-idx)
  (let ((clist '()) (tmp 0))
    (loop for i from 0 to (1- crow-idx) do
        (cond 
          ((or (zerop i) (= i (1- crow-idx)))
            (setf clist (append clist (list 1))))
          (t          
            (setf clist (append clist (list (+ (nth (1- i) prow) (nth i prow))))))
          ))          
    clist
    ))     

 (defun rows (count)
  (when (> count 0)
    (do* ((cc 1 (1+ cc))
        (pt nil (append pt (list  crow)))          
        (prow pt crow)          
        (crow nil ) 
        )
      (( > cc count) pt)
      (setf crow (get-new-row prow cc))
      )
    ))  



    