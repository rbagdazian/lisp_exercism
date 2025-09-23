(defpackage :rna-transcription
  (:use :cl)
  (:export :to-rna))
(in-package :rna-transcription)

(defun to-rna (str) 
  (loop for char across str collecting 
        ((lambda (c)             
              (case c
              (#\G #\C)
              (#\C #\G)
              (#\T #\A)
              (#\A #\U)
              (t (error "Illegal character in string")))) char ) into result 
        finally (return (coerce result 'string))))
                                            

