(defpackage :etl
  (:use :cl)
  (:export :transform))

(in-package :etl)

(defun transform (data)
  "Transforms hash values into keys with their keys as their values."
  (let ((ht (make-hash-table)))
    (maphash #'(lambda (key value) 
        (mapcar (lambda (x) (setf (gethash (char-downcase x) ht) key)) value))
          data)
    ht) 
  )
