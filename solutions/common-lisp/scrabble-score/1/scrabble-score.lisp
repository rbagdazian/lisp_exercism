(defpackage :scrabble-score
  (:use :cl)
  (:export :score-word))

(in-package :scrabble-score)

(defun score-word (word)
  (genscore word 0))

(defvar *plist*  
  (list (cons '(#\A #\E #\I #\O #\U #\L #\N #\R #\S #\T)  1)
        (cons '(#\D #\G)  2)
        (cons '(#\B #\C #\M #\P)  3)
        (cons '(#\F #\H #\V #\W #\Y)  4)
        (cons '(#\K)  5)
        (cons '(#\J #\X)  8)
        (cons '(#\Q #\Z)  10)))

 (defun genscore (seq acc)
  (if (= (length seq) 0)
      acc
      (do* ((cl *plist* (cdr cl))
        (cc (caar cl) (caar cl))
        (ps (cdar cl) (cdar cl))        
        (ts 0))
        ((or (null cl) (plusp ts)) (genscore (subseq seq 1) (+ acc ts)))
        (format t "cc:~A ps:~A ts:~A~%" cc ps ts )
        (if (member (char-upcase (char seq 0)) cc) 
            (setf ts ps))            
      ))
  )

                    
