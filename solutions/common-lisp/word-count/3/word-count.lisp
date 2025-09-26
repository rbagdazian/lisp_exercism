(defpackage :word-count
  (:use :cl)
  (:export :count-words))
(in-package :word-count)


(defun match (char)
  (find char " ,.:!@#$%^&
"))

(defun string-split (str)
  (loop for i = 0 then end
        as start = (position-if #'alphanumericp str :start i)
        while start
        as end = (position-if #'match str :start start)
        collect (string-right-trim "'" (subseq str start end))
        while end))


(defun add-to-alist (l)
  " this function looks for the car of the list l in the hash table
   if found it add one to the associated value and replaces it in the hash table
   otherwise it creates the entry with a 1 associated value"
  (if (null l)                               ; all keywords have been consumed.
      *alst*                                 ; return final version of alst
    ;otherwise
    (let* ((key (string-trim '(#\') (string-downcase (first l)))) ; get current keyword
           (remaining (rest l))              ; get remainder of keyword list   
           (curval (assoc key *alst* :test #'string-equal ))) ; get current value of keyword
      (if (> (length key) 0) ; first make sure key length > 0
        (progn
          (if (null curval) ; check to see if keyword was ever seen.
              (setq *alst* (acons key 1 *alst*)) ; if not create new alist entry with value 1
              (setf (cdr curval) (1+ (cdr curval))) ;otherwise increment the current alist entry
          )))
      (add-to-alist remaining)) ; call self recursively to go to next keyword in input list 
  )
)

(defun count-words (sentence)
  (defparameter *alst* '())
  (add-to-alist (string-split sentence))
  (setq *alst* (reverse *alst*))
  ) 

