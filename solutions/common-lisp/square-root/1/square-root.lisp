(defpackage :square-root
  (:use :cl)
  (:export :square-root))

(in-package :square-root)

(defun square-root (radicand)
  (do ((Xn 100    (* 0.5 (+ Xn (/ radicand Xn)))))
       ((< (abs (- (* Xn Xn) radicand)) 1.0e-6) (round Xn))
    ))
