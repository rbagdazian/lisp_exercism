(defpackage :atbash-cipher
  (:use :cl)
  (:export :encode :decode))

(in-package :atbash-cipher)


(defvar *rkey* "abcdefghijklmnopqrstuvwxyz")
(defvar *key* (reverse *rkey*))

(defun translate ( dir key str )
  ;(format t "key: ~A~%" key)  
  (do (
        (newstr "" newstr)        
        (idx 0 (1+ idx))
        (pt) 
        (cc)
        )
        ((>= idx (length str)) newstr)
        (setf pt (elt str idx))
        (if (eql dir :encode)
          (setf cc (- (char-code pt) (char-code #\a)))
          (setf cc (- 25 (- (char-code pt) (char-code #\a)))))            
        ;; handle numeric characters correctly
        (if (digit-char-p pt) 
           (setf newstr (concatenate 'string newstr (string pt)))
           (setf newstr (concatenate 'string newstr (string (elt key cc))))
        )
        ;; insert spaces appropriately if encoding
        (if (eql dir :encode)
          (if (= (mod (1+ idx) 5) 0) (setf newstr (concatenate 'string newstr " ")))
            )
        ;(format t "idx:~a pt:~a cc:~a newstr:~A ~%" idx pt cc newstr)       
        )) 



(defun encode (plaintext)
  (let ((pre-encode nil) (post-encode nil))
       (setf pre-encode (remove-if-not #'alphanumericp plaintext))
       (setf pre-encode (string-downcase pre-encode))
       (setf post-encode (translate :encode *key* pre-encode ))
       (setf post-encode  (string-trim '(#\Space) post-encode))
       ))

(defun decode (ciphertext)
  (let ((pre-decode nil) (post-decode nil))
       (setf pre-decode (remove-if-not #'alphanumericp ciphertext))
       (setf post-decode (translate :decode *rkey* pre-decode ))       
       ))
      
