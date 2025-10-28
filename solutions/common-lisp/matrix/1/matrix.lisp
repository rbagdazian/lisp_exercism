(defpackage :matrix
  (:use :cl)
  (:export :row
           :column))

(in-package :matrix)

(defun get-integers-from (s &aux (l nil))
  (setf l (uiop:split-string s :separator '(#\Space #\Tab)))
  (mapcar #'parse-integer l)
  )

(defun row (input-matrix index &aux (rowlist nil))
  (setf rowlist (uiop:split-string input-matrix :separator '(#\Newline)))
  (if (zerop (length input-matrix)) 
      nil     
      (let* ((rows (length rowlist))
            (currow nil)
            (res nil)  )
           (when (and (> index 0) (<= index rows))            
             (setf currow (nth (1- index) rowlist)))
             (setf res (get-integers-from currow))             
             )))
          
(defun column (input-matrix index &aux (rowlist nil))
  (setf input-matrix (string-trim '(#\space #\newline #\tab) input-matrix))
  (setf rowlist (uiop:split-string input-matrix :separator '(#\Newline)))
  (if (zerop (length rowlist)) 
        nil
        (let* ((cols (length (get-integers-from (car rowlist))))
              (rows (length rowlist))
              (curcol nil)
              (res nil))
             (when (and (> index 0) (<= index cols))
               (setf curcol '())
               (loop for i from 1 to rows do
                 (setf curcol (append curcol
                   (list (nth (1- index) (get-integers-from (nth (1- i) rowlist))))))))
             curcol
             )))       