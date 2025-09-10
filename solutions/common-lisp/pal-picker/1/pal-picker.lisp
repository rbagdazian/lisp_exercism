(defpackage :pal-picker
  (:use :cl)
  (:export :pal-picker :habitat-fitter :feeding-time-p
           :pet :play-fetch))

(in-package :pal-picker)

(defun pal-picker (personality)
  (case personality
    (:lazy "Cat")
    (:energetic "Dog")
    (:quiet "Fish")
    (:hungry "Rabbit")
    (:talkative "Bird")
    (otherwise "I don't know... A dragon?")))

(defun habitat-fitter (weight)
  (cond ((<= weight 0) :just-your-imagination)
        ((and (>= weight 1) (<= weight 9)) :small)
        ((and (>= weight 10) (<= weight 19)) :medium)
        ((and (>= weight 20) (<= weight 39)) :large)
        ((>= weight 40) :massive)))

(defun feeding-time-p (fullness)
  (cond ((> fullness 20) "All is well.")
        (T "It's feeding time!")))

(defun pet (pet)
  (cond ((string= pet "Fish") "Maybe not with this pet...")
        (T nil)))

(defun play-fetch (pet)
  (cond ((string= pet "Dog") nil)
        (T "Maybe not with this pet...")))

