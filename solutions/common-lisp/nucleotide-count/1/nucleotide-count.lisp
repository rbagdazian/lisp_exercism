(defpackage :nucleotide-count
  (:use :cl)
  (:export :nucleotide-counts))

(in-package :nucleotide-count)

 (defun nucleotide-counts (strand)
  (let ((clst nil) (tchar 0) (Q nil))
      (setq clst (acons #\T 0 clst))
      (setq clst (acons #\G 0 clst))
      (setq clst (acons #\C 0 clst))
      (setq clst (acons #\A 0 clst))       
      ;(format t "initialized clist: ~A~%" clst)       
      (if (> (length strand) 0)       
        ;(format t "entering do loop, clist: ~A~%" clst)
         (do ((idx 0 (1+ idx)))
             ((= idx  (length strand)) clst)
              (setf tchar (char-upcase (char strand idx)))
              (cond ((member tchar '(#\C #\A #\G #\T))
                    (setf (cdr (assoc tchar clst)) (1+ (cdr (assoc tchar clst)))))              
                    (t (setf Q t)))
         ))
       (format t "~A~%" clst)         
       (if Q nil
           clst)
       ))

