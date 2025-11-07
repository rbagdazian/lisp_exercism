(defpackage :house
  (:use :cl)
  (:export :recite))

(in-package :house)


(defparameter *flines*
  '(
    ""
    "This is the house"
    "This is the malt"
    "This is the rat"
    "This is the cat"
    "This is the dog"
    "This is the cow with the crumpled horn"
    "This is the maiden all forlorn"
    "This is the man all tattered and torn"
    "This is the priest all shaven and shorn"
    "This is the rooster that crowed in the morn"
    "This is the farmer sowing his corn"
    "This is the horse and the hound and the horn"  
    )
  )

(defparameter *lines*  
  '( 
    ""
    "that Jack built."
    "that lay in the house"
    "that ate the malt"
    "that killed the rat"
    "that worried the cat"
    "that tossed the dog"
    "that milked the cow with the crumpled horn"
    "that kissed the maiden all forlorn"
    "that married the man all tattered and torn"
    "that woke the priest all shaven and shorn"
    "that kept the rooster that crowed in the morn"    
    "that belonged to the farmer sowing his corn"
    )
  )

(defun build-verse (n v) 
  (if (= n 0)
      (let ((s (make-string-output-stream))) 
           (format s "~{~A~^ ~}" v) (get-output-stream-string s))
      (when (> n 0)
        (setf v (append v (list  (nth n *lines*))))
        (build-verse (1- n) v) )))


(defun recite (start-verse end-verse)
  (let ((verse "")
        (verselist '()))
    (loop for v from start-verse to end-verse do
          (setf verse (list  (build-verse v (list (nth v *flines*)))))
          (setf verselist  (append verselist  verse)))
    (format nil "~{~A~^~%~}" verselist)    
    ))



        



  
  
