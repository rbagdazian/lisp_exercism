(defpackage :pascals-triangle
  (:use :cl)
  (:export :rows))

(in-package :pascals-triangle)

 (defun get-prior-elements (prow crow-idx)
  (let ((clist '()) (tmp 0))
    (loop for i from 0 to (1- crow-idx) do
        (cond 
          ((zerop i) (setf clist (append clist (list 1))))
          ((< i (1- crow-idx))
            (setf tmp (+ (nth (1- i) prow) (nth i prow)))
            (setf clist (append clist (list tmp))))
          (t (setf clist (append clist (list 1)))))           
          )
    clist
    ))     

 
 (defun rows (count)
  (when (> count 0)
    (let* ((pt (list '(1))) (prow pt) (crow nil) (cc 2))
         (when (> count 1)                 
               (loop while (<= cc count) do
                     (setf crow (get-prior-elements prow cc))
                     (setf pt (append pt (list  crow)))
                     (setf prow crow)
                     (setf cc (1+ cc))
                     )                            
           )
      pt
      )
    ))

    