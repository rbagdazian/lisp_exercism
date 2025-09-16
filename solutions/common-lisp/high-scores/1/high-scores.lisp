(defpackage :high-scores
  (:use :cl)
  (:export :make-high-scores-table :add-player
           :set-score :get-score :remove-player))

(in-package :high-scores)

;; Define make-high-scores-table function
(defun make-high-scores-table ()
  (make-hash-table))

;; Define add-player function
(defun add-player (hash-table name) 
  (setf (gethash name hash-table ) 0))
  
;; Define set-score function
(defun set-score (htab name score)
  (setf (gethash name htab ) score))

;; Define get-score function
(defun get-score (htab name)
  (let ( (score (gethash name htab)))
       (cond ((not score) 0)
         (T score))))



;; Define remove-player function
(defun remove-player (htab name)
  (remhash name htab))
