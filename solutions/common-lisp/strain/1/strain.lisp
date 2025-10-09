(defpackage :strain
  (:use :cl)
  (:export :keep :discard))

(in-package :strain)

(defun dofilt (inv sel el acc) 
  (cond ((null sel) acc)
    ((and (null (car sel)) (null inv)) (dofilt inv(cdr sel) (cdr el) acc))
    ((and (car sel) inv) (dofilt inv (cdr sel) (cdr el) acc))
    (t (dofilt inv (cdr sel) (cdr el) (append acc (list (car el)))))
  ))

(defun keep (keep-p elements)
  "Returns a sublist of elements according to a given predicate."
  (let ((filtlist nil) (acc nil))
       (setf filtlist (mapcar keep-p elements))
       (dofilt nil filtlist elements acc)
       ))

(defun discard (discard-p elements)
  "Returns a sublist of elements not matching a given predicate."
  (let ((filtlist nil) (acc nil))
       (setf filtlist (mapcar discard-p elements))
       (dofilt t filtlist elements acc)
       ))
