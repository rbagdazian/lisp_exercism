(defpackage :matching-brackets
  (:use :cl)
  (:export :pairedp))

(in-package :matching-brackets)

(defparameter *pairs* (pairlis (list #\( #\[ #\{) (list #\) #\] #\})))

(defun pairedp (value &aux stack)
  (loop for ch across value
        do (cond ((assoc ch *pairs*) (push ch stack))
                 ((let ((x (car (rassoc ch *pairs*))))
                    (and x (not (eql x (pop stack))))) (return)))
        finally (return (null stack))))

  