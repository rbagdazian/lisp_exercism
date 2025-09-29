(defpackage :space-age
  (:use :cl)
  (:export :on-mercury
           :on-venus
           :on-earth
           :on-mars
           :on-jupiter
           :on-saturn
           :on-uranus
           :on-neptune))

(in-package :space-age)

(defun round-to-decimal-places (number places)
  (let* ((factor (expt 10.0 places))
         (scaled-number (* number factor)))
    (/ (round scaled-number) factor)))

(defun full-accuracy (age)
  (/ age 31557600))

(defun space-age:on-earth (age)
  " convert seconds to years "
  (round-to-decimal-places (full-accuracy age) 2)
  )

(defun space-age:on-mercury (age)
  (round-to-decimal-places (/ (full-accuracy age) 0.2408467) 2)
  )

(defun space-age:on-venus (age)
  (round-to-decimal-places (/ (full-accuracy age) 0.61519726) 2)
  )

(defun space-age:on-mars (age)
  (round-to-decimal-places (/ (full-accuracy age) 1.8808158) 2)
  )

(defun space-age:on-jupiter (age)
  (round-to-decimal-places (/ (full-accuracy age) 11.862615) 2)
  )

(defun space-age:on-saturn (age)
  (round-to-decimal-places (/ (full-accuracy age) 29.447498) 2)
  )

(defun space-age:on-uranus (age)
  (round-to-decimal-places (/ (full-accuracy age) 84.016846) 2)
  )

(defun space-age:on-neptune (age)
  (round-to-decimal-places (/ (full-accuracy age) 164.79132) 2)
  )