(defpackage :matrix
  (:use :cl :uiop)
  (:export :row
           :column))

(in-package :matrix)

(defun build-matrix (input)
  (let ((rows (split-string (stripln input) :separator '(#\Newline))))
    (loop for row in rows
          collect (mapcar #'parse-integer (split-string row)))))

(defun row (input-matrix index)
  (let ((matrix (build-matrix input-matrix)))
    (nth (1- index) matrix)))

(defun column (input-matrix index)
  (let* ((matrix (build-matrix input-matrix))
         (num-rows (length matrix)))
    (loop for r from 0 below num-rows
          collect (nth (1- index) (nth r matrix)))))

        