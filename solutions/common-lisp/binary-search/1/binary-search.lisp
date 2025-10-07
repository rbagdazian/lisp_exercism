(defpackage :binary-search
  (:use :cl)
  (:export :binary-find :value-error))

(in-package :binary-search)

(defun binary-find (arr el)
  (if (zerop (length arr)) nil 
      (do* ((pc 0 (1+ pc) )
            (sarr (sort arr #'<))
            (sidx (loop for i from 0 to (1- (length sarr)) collect i))
            (mididx (floor (/ (length sarr) 2)) (floor (/ (length sarr) 2)) )
            (midval (aref sarr mididx) (aref sarr mididx)))
           ((or  (eql midval el ) (= (length sidx) 1)) (if (= (aref sarr mididx) el) (nth mididx sidx) nil))
        (cond ((> midval el)
               (setf sarr (subseq sarr 0 mididx))
               (setf sidx (subseq sidx 0 mididx)))
              (t 
               (setf sarr (subseq sarr mididx))
               (setf sidx (subseq sidx mididx))))
        ))
  )