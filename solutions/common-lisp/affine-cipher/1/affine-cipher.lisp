(defpackage :affine-cipher
  (:use :cl)
  (:export :encode
           :decode))

(in-package :affine-cipher)

(defun encrypt (p a b m)
  (mod (+ (* a p) b) m))

(defvar *mmi* nil)

(defun getmmi (a m)
  (let ((fv nil))
    (loop for x from 0 to 25 do
            (if (zerop (- (mod (* a x) m) 1))
                (setf fv x))
            )
    fv
    ))

(defun decrypt (c a b m)
  (if (null *mmi*) (setf *mmi* (getmmi a m)))
  (mod (* *mmi* (- c b)) 26))
  

(defun translate ( dir ka kb str )
  (setf *mmi* '())  
  (do (
        (newstr "" newstr)        
        (idx 0 (1+ idx))
        (pt) (pti) 
        (cc)
        )
        ((>= idx (length str)) newstr)
    
        (setf pt (elt str idx))
        (setf pti (- (char-code pt) (char-code #\a)) )
        (if (eql dir :encode)
          (setf cc (encrypt pti ka kb 26))
          (setf cc (decrypt pti ka kb 26))
        )

        ;; handle numeric characters correctly
        (if (digit-char-p pt) 
           (setf newstr (concatenate 'string newstr (string pt)))
           (setf newstr (concatenate 'string newstr 
                                     (string (code-char (+ cc (char-code #\a))))))
        )
        ;; insert spaces appropriately if encoding
        (if (eql dir :encode)
          (if (= (mod (1+ idx) 5) 0) (setf newstr (concatenate 'string newstr " ")))
            )
        )) 

(defun coprimep (a m)
  (if (= (gcd a m) 1) t nil))

(defun encode (plaintext a b)
  (if (coprimep a 26)
  (let ((pre-encode nil) (post-encode nil))
       (format t "~A ~A ~A ~%" plaintext a b)
       (setf pre-encode (remove-if-not #'alphanumericp plaintext))
       (format t "~A ~%" pre-encode)
       (setf pre-encode (string-downcase pre-encode))
       (format t "~A ~%" pre-encode)
       (setf post-encode (translate :encode a b pre-encode ))
       (setf post-encode  (string-trim '(#\Space) post-encode))
       )
      nil
      ))

(defun decode (ciphertext a b)
  (if (coprimep a 26)
  (let ((pre-decode nil) (post-decode nil))
       (setf pre-decode (remove-if-not #'alphanumericp ciphertext))
       (setf post-decode (translate :decode a b  pre-decode ))
       )
  nil
      ))