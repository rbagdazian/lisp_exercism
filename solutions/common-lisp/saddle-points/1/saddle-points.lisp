(defpackage :saddle-points
  (:use :cl)
  (:export :saddle-points))

(in-package :saddle-points)

(defun saddle-points (matrix)
  " find points in the matrix where the largest 
  value in a given row is the smallest value 
  in its corresponding column"
    
  (let* ((rows (array-dimension matrix 0))
         (cols (array-dimension matrix 1))
         (res nil)
         )
    (dotimes (row rows)
      (dotimes (col cols)
      (if (= (max-of-row matrix row) (min-of-col matrix col))
          (setf res (append res (list (list (1+ row) (1+ col)))))
          ) 
      ))      
    res
    )
  )

(defun max-of-row (arr row &aux (maxv -1000)) 
  (dotimes (col (array-dimension arr 1))
    (if (> (aref arr row col) maxv)
      (setf maxv (aref arr row col))))
  maxv)

(defun min-of-col (arr col &aux (minv 1000)) 
  (dotimes (row (array-dimension arr 0))
    (if (< (aref arr row col) minv)
      (setf minv (aref arr row col))))
  minv)


