(defpackage :pig-latin
  (:use :cl)
  (:export :translate))

(in-package :pig-latin)

(defun tackay (phrase) (concatenate 'string phrase "ay"))

(defun assemble (prefix suffix) (concatenate 'string suffix prefix "ay"))

(defun splitphrase (phrase acc)
    (let ((spaceloc (position-if #'(lambda (char) (member char '(#\space))) phrase)))
       (cond ((null spaceloc) (append acc (list  (string-trim '(#\space) phrase))))
             (t   (setf acc (append acc (list (string-trim '(#\space) (subseq phrase 0 spaceloc)))))
                  (splitphrase (string-trim '(#\space) (subseq phrase spaceloc)) acc))
         )))

(defun re-phrase (wordlist acc)
  (cond ((null wordlist) (string-trim '(#\space)  acc))
        (t (re-phrase (cdr wordlist) (concatenate 'string acc " " (car wordlist))))
        )
  )

(defun translate (phrase)
    (let ((wordlist (splitphrase phrase '()))
          (translated-list nil)
         )
         (setf translated-list (mapcar #'translate-word wordlist))
         (re-phrase translated-list '())
         ))


(defun translate-word (word)  
  (let ((first-vowel (position-if #'(lambda (char) (member char '(#\a #\e #\i #\o #\u))) word))
        (first-two (subseq word 0 2))
        (qupos (search "qu" word))
        (ypos (search "y" word))
        (prefix "") (suffix "")
        )
       (cond ; Rule 1
         ((and first-vowel (= first-vowel 0)) (tackay word))
         ((string-equal first-two "xr") (tackay word))
         ((string-equal first-two "yt") (tackay word))
         ((and ypos (null first-vowel))
             (setf prefix (subseq word 0 ypos))
             (setf suffix (subseq word ypos))
             (assemble prefix suffix))
             ;Rule 2
          ((or (and (> first-vowel 0) (null qupos)) (and qupos (< first-vowel qupos)))
             (setf prefix (subseq word 0 first-vowel))
             (setf suffix (subseq word first-vowel))
             (assemble prefix suffix))           
             ; Rule 3
         ((and qupos (> first-vowel qupos))
             (setf prefix (subseq word 0 (+ qupos 2)))
             (setf suffix (subseq word (+ qupos 2)))
             (assemble prefix suffix)) 
             ; Rule 4
         ((and ypos (> first-vowel ypos))
             (setf prefix (subseq word 0 ypos))
             (setf suffix (subseq word ypos))
             (assemble prefix suffix))
         (t (format t "Don't know what to do with: ~A~%" word)))         
       ))
          
        
