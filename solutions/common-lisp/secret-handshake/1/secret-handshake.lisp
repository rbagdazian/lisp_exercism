(defpackage :secret-handshake
  (:use :cl)
  (:export :commands))

(in-package :secret-handshake)

(defun commands (number)
  (let ((actions nil) (do-reverse nil))
            (loop for i from 0 to 4 do
             (if (= (logand 1 number) 1)
                 (setf actions (append actions
                  (list (case i
                   ((0) "wink")
                   ((1) "double blink")
                   ((2) "close your eyes")
                   ((3) "jump")
                   ((4) (setf do-reverse t)))))))
            (setf number (ash number -1)))
      (when do-reverse 
        (setf actions (reverse actions)) 
        (setf actions (cdr actions)))
      actions))
