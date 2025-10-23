(defpackage :matching-brackets
  (:use :cl)
  (:export :pairedp))

(in-package :matching-brackets)

(defun process-char (c cstack)
  (format t "entry: c: ~a cstack: ~a ~%" c cstack)  
  (let ((res t))
    (cond
      ((char-equal c (char "(" 0))  (setf cstack (push c cstack)))
      ((char-equal c (char "[" 0)) (setf cstack (push c cstack)))
      ((char-equal c (char "{" 0)) (setf cstack (push c cstack)))
      ((char-equal c (char "}" 0)) (if  (setf res (and cstack  (char-equal (car cstack) (char "{" 0))))
                                        (setf cstack (cdr cstack))) )
      ((char-equal c (char "]" 0)) (if  (setf res (and cstack  (char-equal (car cstack) (char "[" 0))))
                                        (setf cstack  (cdr cstack))) )
      ((char-equal c (char ")" 0)) (if  (setf res (and cstack  (char-equal (car cstack) (char "(" 0))))
                                        (setf cstack (cdr cstack))) )
      (t t)
      )
    (format t "res: ~a cstack: ~a ~%" res cstack)
    (values res cstack)))
    


(defun pairedp (value)
  (if (zerop (length value))
      t ; handle null input string
      ;else
      (let ((res t) (cstack '()) (c 0))
        (with-input-from-string (s value)
          (loop while (peek-char nil s nil) do
                (setf c (read-char s))
                (multiple-value-bind (rr cstk) (process-char c cstack)
                  (setf cstack cstk)
                  (setf res rr)
                  )
                (format t "stack: ~A ~A res; ~A~%" cstack (car cstack) res)
                (if (null res) (return))
                ))
        (if cstack (setf res nil))
        res)
      )
    )

  