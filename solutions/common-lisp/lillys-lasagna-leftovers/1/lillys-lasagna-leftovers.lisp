(defpackage :lillys-lasagna-leftovers
  (:use :cl)
  (:export
   :preparation-time
   :remaining-minutes-in-oven
   :split-leftovers))

(in-package :lillys-lasagna-leftovers)

;; Define function preparation-time
(defun preparation-time (&rest layers)
  "compute preparation time based on number of layers"
  (* 19 (length layers)))

;; Define function remaining-minutes-in-oven
(defun remaining-minutes-in-oven (&optional (cook-time :normal ct-supplied-p))
  "determine cooking time based on optional arg"
  (if (not ct-supplied-p) 337
    (case cook-time
      ((:NORMAL) 337)
      ((:SHORTER) 237)
      ((:VERY-SHORT) 137)
      ((:LONGER) 437)
      ((:VERY-LONG) 537)
      ((nil) 0))))

;; Define function split-leftovers
(defun split-leftovers (&key (weight 0 w-supplied-p) (human 10) (alien 10))
  "determine the amount of left-overs"
  ;(format t "weight: ~A ~A~%" weight (null weight))
  (cond ((not w-supplied-p) :just-split-it)
        ((null weight) :looks-like-someone-was-hungry)
        ((null human) nil)
        ((null alien) nil)
        (t (- weight (+ human alien)))))

