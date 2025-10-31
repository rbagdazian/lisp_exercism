(defpackage :secret-handshake
  (:use :cl)
  (:export :commands))

(in-package :secret-handshake)

(defun commands (number)
  (loop for i below 4 when (logbitp i number)
   collect (nth i '("wink" "double blink" "close your eyes" "jump")) into result
   finally (return (if (logbitp 4 number) (reverse result) result))))
