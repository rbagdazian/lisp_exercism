(defpackage :rotational-cipher
  (:use :cl)
  (:export :rotate))

(in-package :rotational-cipher)

(defun encode-char (ch key)
  (let ((bc 0))        
    (cond ((upper-case-p ch)
           (setf bc (char-code #\A))
           (code-char (+ (mod (+ (- (char-code ch) bc) key) 26) bc)))      
          ((lower-case-p ch)
           (setf bc (char-code #\a))
           (code-char (+ (mod (+ (- (char-code ch) bc) key) 26) bc)))      
          (t 
           ch))))
              
(defun rotate (text key)
  (coerce (loop for ch across text 
        collect (encode-char ch key)) 'string))
  
        
  
