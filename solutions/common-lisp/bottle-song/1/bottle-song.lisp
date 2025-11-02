(defpackage :bottle-song
  (:use :common-lisp)
  (:export :recite))

(in-package :bottle-song)

(defun get-verse (n)
  (list 
    (format nil "~@(~r~) green bottle~:p hanging on the wall," n)
    (format nil "~@(~r~) green bottle~:p hanging on the wall," n)
    (format nil "And if one green bottle should accidentally fall,")
    (if (> n 1)
      (format nil "There'll be ~r green bottle~:p hanging on the wall." (1- n))
      (format nil "There'll be no green bottles hanging on the wall.")
    ))
  )

(defun recite (start-bottles take-down)
  "Returns the song verses from START-BOTTLES down to (- START-BOTTLES TAKE-DOWN)."
  (do ((sb start-bottles (1- sb))
       (td take-down (1- td))
       (vlist (list) ))
      ((= td 0) vlist)
      (setf vlist (append vlist    (get-verse sb) ))
      (if (and (> sb 1) (> td 1))
      (setf vlist (append vlist (list ""))))
    ))
