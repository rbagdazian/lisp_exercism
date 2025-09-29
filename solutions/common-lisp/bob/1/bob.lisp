(defpackage :bob
  (:use :cl)
  (:export :response))
(in-package :bob)

(defun all-caps-p (str)
  (and (some #'(lambda (c) (char>= (char-upcase c) #\A )) str)
       (some #'(lambda (c) (char<= (char-upcase c) #\Z )) str)
       (string= str (string-upcase str)))
  )

(defun question-p (str)
  (let* ((last-char-index (1- (length str)))
     (last-char (char str last-char-index)))
     (char-equal last-char #\?))) 

(defun has-noncaseonly-p (str)
  (cond ((= (length str) 0) T)
    ((and (char>= (char-downcase (char str 0)) #\a )
          (char<= (char-downcase (char str 0)) #\z )) nil)
    (T (has-noncaseonly-p (subseq str 1)))
    ))

(defun response (hey-bob)
  (let ((strv (string-trim '(#\Space #\Tab #\Newline #\Return) hey-bob)))
    (cond
        ((= (length strv) 0) "Fine. Be that way!") 
        ((and (all-caps-p strv)
             (question-p strv))
            "Calm down, I know what I'm doing!")
        ((question-p strv) "Sure.")
        ((or (and (has-noncaseonly-p strv) (question-p strv)) 
         (has-noncaseonly-p strv)) "Whatever.")
        ((all-caps-p strv) "Whoa, chill out!")
        (T "Whatever."))
    ))
  


