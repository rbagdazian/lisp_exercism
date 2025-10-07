(defpackage :isogram
  (:use :cl)
  (:export :isogram-p))

(in-package :isogram)


(defun isogram-p (string)
  "Is string an Isogram?"
  (let ((res t) (clist '()) (asi nil) (tstr ""))
    (setf tstr (remove #\Space string))
    (setf tstr (remove #\- tstr))
    (setf tstr (remove #\' tstr))
    (loop for i from 0 to (1- (length tstr)) do
        (if (assoc (char tstr i) clist :test #'char-equal )
            (setf res nil)
            (setq clist (acons (char tstr i) 1 clist)))
        (if (null res) (return))
        )
    res
    )
  )

  
