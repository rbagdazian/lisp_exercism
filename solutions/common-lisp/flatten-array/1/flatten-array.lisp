(defpackage :flatten-array
  (:use :cl)
  (:export :flatten))

(in-package :flatten-array)

(defun doflat (nested &optional acc)
  (cond ((null nested)  acc)
    ((listp (car nested)) 
         (setf acc (doflat (car nested) acc))
         (doflat (cdr nested) acc))
         (t (setf acc (append acc (list (car nested))))
            (setf acc (doflat (cdr nested) acc))
            ))
  )

(defun flatten (nested)
  (doflat nested))
