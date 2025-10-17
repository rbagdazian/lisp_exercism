(defpackage :robot-name
  (:use :cl)
  (:export :build-robot :robot-name :reset-name))

(in-package :robot-name)

(defvar *names* nil)

(defun build-robot ()
  (if (null *names*) (setf *names* (make-hash-table)))
  (let ((finalname ""))
    (do* (
          (char1 (string (code-char  (+ (random 26) (char-code #\A)))) 
                 (string (code-char  (+ (random 26) (char-code #\A)))))
          (char2 (string (code-char  (+ (random 26) (char-code #\A)))) 
                 (string (code-char  (+ (random 26) (char-code #\A)))))
          (prefix (format nil "~A~A" char1 char2) 
                  (format nil "~A~A" char1 char2))
          (suffix (write-to-string (random 1000)) 
                  (write-to-string (random 1000)))
          (newname (concatenate 'string prefix suffix) 
                   (concatenate 'string prefix suffix))
          )
         ((null (gethash newname *names*)) 
             (setf finalname newname) 
             (setf (gethash newname *names*) newname))
         (format t "hash collision on ~A, trying again~%" newname)
      )
    (format t "finalname: ~A~%" finalname)
    finalname
    ))

(defun robot-name (robot)
  (gethash robot *names*)
  )

(defun reset-name (robot)
  (remhash robot *names*)
  t
  )


       
          
       


