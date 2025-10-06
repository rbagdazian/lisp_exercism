(defpackage :twelve-days
  (:use :cl)
  (:export :recite))

(in-package :twelve-days)



(defparameter *verses*
  (list 
   (cons 0 "On the ~A day of Christmas my true love gave to me: ") 
   (cons 12 "~A Drummers Drumming, ") 
   (cons 11 "~A Pipers Piping, " )
   (cons 10 "~A Lords-a-Leaping, ")
   (cons 9 "~A Ladies Dancing, ")
   (cons 8 "~A Maids-a-Milking, ")
   (cons 7 "~A Swans-a-Swimming, ")
   (cons 6 "~A Geese-a-Laying, ")
   (cons 5 "~A Gold Rings, ")
   (cons 4 "~A Calling Birds, ") 
   (cons 3 "~A French Hens, ")
   (cons 2 "~A Turtle Doves, ")
   (cons 1 "~A Partridge in a Pear Tree.")
   ))

(defparameter *ordinals* 
  (list 
   (cons 1 "first")
   (cons 2 "second")
   (cons 3 "third")
   (cons 4 "fourth")
   (cons 5 "fifth")
   (cons 6 "sixth")
   (cons 7 "seventh")
   (cons 8 "eighth")
   (cons 9 "ninth")
   (cons 10 "tenth")
   (cons 11 "eleventh")
   (cons 12 "twelfth")
   ))

(defparameter *numnames* 
  (list 
   (cons 1 "a")
   (cons 2 "two")
   (cons 3 "three")
   (cons 4 "four")
   (cons 5 "five")
   (cons 6 "six")
   (cons 7 "seven")
   (cons 8 "eight")
   (cons 9 "nine")
   (cons 10 "ten")
   (cons 11 "eleven")
   (cons 12 "twelve")
   ))


(defun recite (&optional begin end)
  "Returns a string of the requested verses for the 12 Days of Christmas."
  (if (and (null begin) (null end)) (progn (setf begin 1) (setf end 12)))
  (if (null end) (setf end begin))  
  (let ((final 0) (cond-newline (if (> end begin) '(#\Newline) "" )))
    (do* ((day begin (1+ day))
          (verse "" verse)
          (song "" song))       
         ((> day end) song )
      (setf verse (format nil (cdr (assoc 0 *verses*)) (cdr (assoc day *ordinals*))) )
      (if (= day end) (setf cond-newline ""))
      (do* ((curday day (1- curday)))
           ((< curday 1) (setf song  (concatenate 'string song verse cond-newline )))
        (if (and (/= day 1) (= curday 1))
            (setf verse  (concatenate 'string verse "and " (format nil (cdr (assoc curday *verses*)) (cdr (assoc curday *numnames*)))  ))
            (setf verse  (concatenate 'string verse (format nil (cdr (assoc curday *verses*)) (cdr (assoc curday *numnames*)))))   
            )
        )
      )
    )
  )

