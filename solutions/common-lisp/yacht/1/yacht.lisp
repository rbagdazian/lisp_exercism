(defpackage :yacht
  (:use :cl)
  (:export :score))
(in-package :yacht)

(defun score (scores category)
  "Returns the score of the dice for the given category.
  we will copy the incoming scores in to an alist for 
  collecting the number of each pip count from the dies"
  (let ((alist nil)
        (entry nil)
        (res nil))
       (setf alist (copy-alist '((1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 0))))
       (format t "scores: ~A~%" scores)
       (loop for key in scores do
              (incf (cdr (assoc key alist)))
             )
       (format t "alist ~A~%" alist)
       (setf res 0)       
       (case category
         ((:yacht)
          (loop for key from 1 to 6 do
              (when (= (cdr (assoc key alist)) 5 )
                (setf res 50))))
          ((:ones) 
           (setf res (cdr (assoc 1 alist))))
          ((:twos)
           (setf res (* (cdr (assoc 2 alist)) 2)))
          ((:threes)
           (setf res (* (cdr (assoc 3 alist)) 3)))
          ((:fours)
           (setf res (* (cdr (assoc 4 alist)) 4)))
          ((:fives)
           (setf res (* (cdr (assoc 5 alist)) 5)))
         ((:sixes)
           (setf res (* (cdr (assoc 6 alist)) 6)))
         ((:full-house)
          (let ((foo '()))
               (loop for n from 1 to 6 do
                 (if (/= (cdr (assoc n alist)) 0)
                     (setf foo (append foo (list (cdr (assoc n alist)))))))
               (if (or (equal foo '(2 3)) (equal foo '(3 2)))
                   (loop for xx in alist do (setf res (+ res (* (car xx) (cdr xx))))))
               ))
         ((:four-of-a-kind)
               (loop for n from 1 to 6 do
                 (if (>= (cdr (assoc n alist)) 4)
                     (setf res (* n 4)))))
          ((:little-straight)
           (let ((foo (mapcar #'cdr alist)) )
                (if (equal foo '(1 1 1 1 1 0)) (setf res 30))))
           ((:big-straight)
           (let ((foo (mapcar #'cdr alist)) )
                (if (equal foo '( 0 1 1 1 1 1)) (setf res 30)))) 
           ((:choice)
               (loop for xx in alist do (setf res (+ res (* (car xx) (cdr xx))))))     
          )
       res))
       
