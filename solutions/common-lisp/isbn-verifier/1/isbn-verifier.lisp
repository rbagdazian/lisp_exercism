(defpackage :isbn-verifier
  (:use :cl)
  (:export :validp))

(in-package :isbn-verifier)

(defun char-to-num (ch)
  (if (eql ch #\X) 10
      ; otherwise
      (cond ((< (char-code ch) 48) nil)
            ((> (char-code ch) 57) nil)
            (t (- (char-code ch) 48)))))

(defun isbn-check (num fac acc)
    (let* ((fc (char num 0))   ; extract leading character
        (fv (char-to-num fc))) ; compute character code
        (cond ((null fv) -1000) ; illegal character in string
              ((= fac 1) (+ acc fv)) ; final evaluation
              ((and (= fv 10) (/= fac 1)) -1000) ; X appeared in invalid location
              (t (+ acc (+ (* fac fv) (isbn-check (subseq num 1) (1- fac) acc))))) ; next pass
       ))

(defun validp (isbn)
  (let* ((isbn2 (remove #\- isbn))
         (lenv (length isbn2))
         (sumv (if (= lenv 10) (isbn-check isbn2 10 0) -1000)))
         (cond ((< sumv 0) nil) ; error condition detected in isbn-check
               (t (= (mod sumv 11) 0)) ; final check for modulo confirmation
           )))


