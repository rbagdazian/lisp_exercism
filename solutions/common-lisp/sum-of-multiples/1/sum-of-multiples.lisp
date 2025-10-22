(defpackage :sum-of-multiples
  (:use :cl)
  (:export :sum))

(in-package :sum-of-multiples)

 (defun list-of-factor (factor cfac limit lfac)
   " this function returns a list of all multiples of factor up to the limit
   as a list"
       (cond ((null factor) lfac)
             ((zerop factor) nil)
            ((< cfac limit) 
              (list-of-factor factor (+ factor cfac) limit (append lfac (list cfac))))
             (t lfac))
       )

(defun add-list-to-bag (lst bag)
  " this function adds the contents of a list to the set containing only unique
  entries from lst"
  (cond ((null lst) bag)
        (t (setf bag (adjoin (car lst) bag)) (add-list-to-bag (cdr lst) bag)))
  )


(defun sum (factors limit)
  (do* ((lst (list-of-factor (car factors) (car factors) limit '())
             (list-of-factor (car factors) (car factors) limit bag))
        (bag (add-list-to-bag lst '()) 
             (add-list-to-bag lst bag)))
        ((null factors) (reduce #'+ bag))
        (setf factors (cdr factors)) 
    ))
        
    
