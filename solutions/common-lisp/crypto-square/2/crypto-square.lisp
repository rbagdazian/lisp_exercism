(defpackage :crypto-square
  (:use :cl)
  (:export :encipher))
(in-package :crypto-square)

(defun remove-chars-from-string (chars-to-remove input-string)
  (coerce (remove-if (lambda (char)
                       (member char chars-to-remove :test #'char=))
                     (coerce input-string 'list))
          'string))

(defun normalize (text)
  (remove-chars-from-string '(#\Space #\Return #\Newline #\. #\, #\; #\: #\' #\- #\@ #\! #\%  #\" ) text)
  )

(defun fill-array (arr i r c ntxt)
  ;(format t "on entry: arr: ~a ~%" arr)
  ;(format t "on entry: r: ~A c: ~A ~%" r c)
  (let ((ri (floor (/ i c))) (ci (rem i c))) 
      
  ;(format t "i: ~A ri: ~A ci: ~A ~%" i ri ci)
  ;(format t "arr: ~A arrv: ~A c:~A ~%"  arr (aref arr ri ci) (subseq ntxt 0 1))
  (cond ((zerop (length ntxt)) arr)
        (t arr
         (setf (aref arr ri ci) (subseq ntxt 0 1))
         (fill-array arr (1+ i) r c (subseq ntxt 1))
         )
   )
  )) 

(defun read-array (arr idx r c)
  ;(format t "in read-array: ~a ~a ~%" r c)
  (let ((out "") (fin "") (idx 0))
       (loop for ci from 0 to (1- c) do
         (loop for ri from 0 to (1- r) do
               (setf out (concatenate 'string out (aref arr ri ci)))
               (setf idx (1+ idx))
               )
             (if (/= idx (* c r))
                 (setf out (concatenate 'string out " ")))
             )
       out
       )
  )

(defun encode (normtext)
  (let ((c 0) (r 0) (arr nil) (outtxt ""))
  (loop for i from (ceiling (sqrt (length normtext))) downto 1 do
       (setf c i)
       (setf r (floor (/ (length normtext) c)))
       (when (and (>= c r) (<= (- c r) 1))
          (if (< (* c r) (length normtext)) (setf r (1+ r)))
          (if (< c r)  (setq c (prog1 r (setq r c))) )
          (setf arr (make-array (list r c) :initial-element " "))
          (setf arr (fill-array arr 0 r c normtext))
          (setf outtxt (read-array arr 0 r c))
          (return)
        )          
       )
       outtxt 
       ))

(defun encipher (plaintext)
  (let ((normtext (string-downcase (normalize plaintext))))
       (if (= (length normtext) 0) ""
           (encode normtext))
       )) 
  

