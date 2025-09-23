(defpackage :rna-transcription
  (:use :cl)
  (:export :to-rna))
(in-package :rna-transcription)
  
 (defun to-rna (str)
  "Transcribe a string representing DNA nucleotides to RNA."
  (cond ((eql (length str) 0) "")  
        (t 
         (let ((lc (char str 0)) (nt ""))          
            (case lc
              (#\G (setf nt #\C))
              (#\C (setf nt #\G))
              (#\T (setf nt #\A))
              (#\A (setf nt #\U))
              (t (error "Illegal character in string")))
            (format nil "~a~a" nt (to-rna (subseq str 1)))
               ))))

