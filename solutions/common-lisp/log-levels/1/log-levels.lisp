(defpackage :log-levels
  (:use :cl)
  (:export :log-message :log-severity :log-format))

(in-package :log-levels)

(defun log-message (log-string)
  "Takes a log string and returns the appropriate msg"
    (let ((msg (subseq log-string 8))
          (level (subseq log-string 1 5))
          )
         (cond 
           ((string= level "info") msg)
           ((string= level "warn") msg)
           ((string= level "ohno") msg)
           (T nil)
           )
         )
       )

(defun log-severity (log-string)
  "Takes a log string and returns the appropriate msg"
    (let (
          (level (string-downcase (subseq log-string 1 5)))
          )
         (cond 
           ((string= level "info") :everything-ok)
           ((string= level "warn") :getting-worried)
           ((string= level "ohno") :run-for-cover)
           (T nil)
           )
         ) )
       

(defun log-format (log-string)
    "Takes a log string and returns the appropriate msg"
    (let ((msg (subseq log-string 8))
          (level (subseq log-string 1 5))
          )
         (cond 
           ((string= level "info") (string-downcase msg))
           ((string= level "warn") (string-capitalize msg))
           ((string= level "ohno") (string-upcase msg))
           (T nil)
           )))
