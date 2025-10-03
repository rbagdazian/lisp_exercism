(defpackage :acronym
  (:use :cl)
  (:export :acronym))

(in-package :acronym)

(defun acronym (phrase)
  "remove all non-alphanumeric characters from input string"
  (let ((so (with-output-to-string (s)
              (loop for char across phrase
                    when (or  (alphanumericp char) (member char '(#\Space #\Tab #\- #\Newline #\Return )))
                    do (write-char char s))))
        )
    (setf so (string-capitalize so))
    (with-output-to-string (r)
      (loop for char across so
            when (upper-case-p char)
            do (write-char char r)))))  
