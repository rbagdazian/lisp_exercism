(defpackage :food-chain
  (:use :cl)
  (:export :recite))

(in-package :food-chain)


(defvar *objects* '("-" "fly" "spider" "bird" "cat" "dog" "goat" "cow" "horse" ))
(defvar *why* '(
                "I know an old lady who swallowed a ~A."                
                "I don't know why she swallowed the fly. Perhaps she'll die."
                "It wriggled and jiggled and tickled inside her." 
                "How absurd to swallow a bird!"
                "Imagine that, to swallow a cat!"
                "What a hog, to swallow a dog!"
                "Just opened her throat and swallowed a goat!"
                "I don't know how she swallowed a cow!"
                "I know an old lady who swallowed a horse."
                "She's dead, of course!"))

(defun genrepeat (verse n)
  (cond ((= n 3)
         (setf verse (append verse 
                             (list (format nil "She swallowed the ~A to catch the ~A that wriggled and jiggled and tickled inside her." (nth n *objects*) (nth (1- n) *objects*)))))
         (setf verse (append verse
                             (list (format nil "She swallowed the ~A to catch the ~A." (nth 2 *objects*) (nth 1 *objects*)))))
         verse)
        (t  
         (setf verse (append verse  
                             (list (format nil "She swallowed the ~A to catch the ~A." (nth n *objects*) (nth (1- n) *objects*)))))
         (genrepeat verse (1- n)))))         


(defun genverse (n m)
  (let ((verse nil))
  (when (= n 1)
        (setf verse (append verse (list (format nil (nth 0 *why*) (nth n *objects*)))))
        (setf verse (append verse (list (nth 1 *why*)))))          
  (when (= n 2)
        (setf verse (append verse (list (format nil (nth 0 *why*) (nth n *objects*)))))
        (setf verse (append verse (list (format nil "~A" (nth n *why*)))))
        (setf verse (append verse (list (format nil "She swallowed the ~A to catch the ~A." (nth 2 *objects*) (nth 1 *objects*)))))
        (setf verse (append verse (list (nth 1 *why*)))))
  (when (and (> n 2) (< n 8))
        (setf verse (append verse (list (format nil (nth 0 *why*) (nth n *objects*)))))
        (setf verse (append verse (list (format nil "~A" (nth n *why*)))))          
        (setf verse (genrepeat verse n))
        (setf verse (append verse (list (nth 1 *why*))))
        )
  (when (= n 8)  
        (setf verse (append verse (list (format nil "~A" (nth 8 *why*)))))
        (setf verse (append verse (list (format nil "~A" (nth 9 *why*)))))
        verse)
  (if (/= n m) (setf verse (append verse (list ""))))
  (format nil "~{~A~^~%~}" verse)         
  ))

(defun recite (start-verse end-verse)
  (let ((res ""))
    (loop for n from start-verse to end-verse
          collect  (genverse n end-verse) into result
          finally  (setf res result))
    (format nil "~{~A~^~%~}" res) 
    ))
