(defpackage :phone-number
  (:use :cl)
  (:export :clean))

(in-package :phone-number)

(defun remove-chars-from-string (chars-to-remove input-string)
  (coerce (remove-if (lambda (char) (find char chars-to-remove))
                     (coerce input-string 'list))
          'string))



(defun clean (phrase)
  "Converts a PHRASE string into a string of digits.
  Will evaluate to \"0000000000\" in case of an invalid input.
  Need to eliminate \"()-.+ \" from the input string, then
  remove leading '1', then check for numerical only string"
  (let ((numstr "" ))
       (setf numstr (remove-chars-from-string '(#\( #\) #\- #\+ #\. #\ ) phrase ))
       (if (string-equal (subseq numstr 0 2) "11") ; make this string bad
           (setf numstr "0000000000"))
       (if (char-equal (char numstr 0) #\1 ) (setf numstr (subseq numstr 1)))
       (if (/= (length (write-to-string (parse-integer numstr :junk-allowed t))) 10)
           "0000000000"
           (if (or (eql (char numstr 3) #\0) (eql (char numstr 3) #\1))
             "0000000000"
              numstr))          
       ))
