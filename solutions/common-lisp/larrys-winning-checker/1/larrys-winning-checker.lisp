(defpackage :larrys-winning-checker
  (:use :cl)
  (:export
   :make-empty-board
   :make-board-from-list
   :all-the-same-p
   :row
   :column))

(in-package :larrys-winning-checker)

(defun make-empty-board ()
  (make-array '(3 3) :initial-element nil))

(defun make-board-from-list (list)
  (make-array '(3 3) :initial-contents list))

(defun all-the-same-p (row-or-col)
  (and (equal (aref row-or-col 0) (aref row-or-col 1))
       (equal (aref row-or-col 1) (aref row-or-col 2))))

(defun row (board row-num)
  (cond ((not (and (< row-num 3) (>= row-num 0))) nil)
    (t
        (make-array '(3) :initial-contents
              (list (aref board row-num 0) (aref board row-num 1) (aref board row-num 2))))))

  

(defun column (board col-num)
  (cond ((not (and (< col-num 3) (>= col-num 0))) nil)
    (t
        (make-array '(3) :initial-contents
              (list (aref board 0 col-num) (aref board 1 col-num) (aref board 2 col-num))))))
  
