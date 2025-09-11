(defpackage :lillys-lasagna
  (:use :cl)
  (:export :expected-time-in-oven
           :remaining-minutes-in-oven
           :preparation-time-in-minutes
           :elapsed-time-in-minutes))

(in-package :lillys-lasagna)

;; Define function expected-time-in-oven
(defun expected-time-in-oven () 
  "According to tradition" 337)

;; Define function remaining-minutes-in-oven
(defun remaining-minutes-in-oven (x)
  "difference of elapsed time and expected time"
  (- (expected-time-in-oven) x ))

;; Define function preparation-time-in-minutes
(defun preparation-time-in-minutes (layers) 
  "This provides the estimated time based on layers in the lasagna"
  (* layers (/ 57 3)))

;; Define function elapsed-time-in-minutes
(defun elapsed-time-in-minutes (layers time-in-oven)
  "return (cooking-time-time-in-oven) + (preparation-time)"
  (+ time-in-oven (preparation-time-in-minutes layers)))
