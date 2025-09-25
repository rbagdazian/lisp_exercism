(defpackage :roman-numerals
  (:use :cl)
  (:export :romanize))

(in-package :roman-numerals)

(defun get-counts (number)
  (let ((nlist ()) (gq 0) (gr 0))
       (setf gq (floor (/ number 1000))) (setf gr (rem number 1000))
       (setq nlist (append nlist (list gq)))
       (setf gq (floor (/ gr 500))) (setf gr (rem number 500))
       (setq nlist (append nlist (list gq)))
       (setf gq (floor (/ gr 100))) (setf gr (rem number 100))
       (setq nlist (append nlist (list gq)))
       (setf gq (floor (/ gr 50))) (setf gr (rem number 50))
       (setq nlist (append nlist (list gq)))
       (setf gq (floor (/ gr 10))) (setf gr (rem number 10))
       (setq nlist (append nlist (list gq)))
       (setf gq (floor (/ gr 5))) (setf gr (rem number 5))
       (setq nlist (append nlist (list gq)))
       (setq nlist (append nlist (list gr)))))

(defun get-adjust (clist alist rlist) 
       (cond 
         ((null clist) 
              (values  (append (rest alist) '(0)) rlist))
         (t 
           (let ((a 0) (r 0))
             (setf a (floor (/ (first clist) 4))) 
             (setf r (rem (first clist) 4)) 
             (get-adjust (rest clist) (append alist (list a)) (append rlist (list r)))))
         ))

(defun render (alist rlist clist olist)
  (let ((cc (list (first clist))))
       (cond
         ((null alist) olist)
         ((eq (car alist) 1) 
          (setf olist (append olist (list (cadr clist))))
          (setf olist (append olist cc))
          (if (car rlist)
            (loop for i from 1 to (car rlist) do (setf olist (append olist cc))))
          (render (rest alist) (rest rlist) (rest clist)  olist))
         (t
          (if (car rlist)
            (loop for i from 1 to (car rlist) do (setf olist (append olist cc))))
          (render (rest alist) (rest rlist) (rest clist) olist)))))


(defun cleanup (rs)
  (format t "Input to cleanup: ~A~%" rs)
  (let ((q1 (search "IVV" rs)) (q2 (search "XLL" rs)) (q3 (search "CDD" rs)) (sposv nil) (substr nil) (rv nil))
      (cond 
        ((not (null q1)) (setf sposv q1) (setf substr "IX"))
        ((not (null q2)) (setf sposv q2) (setf substr "XC"))
        ((not (null q3)) (setf sposv q3) (setf substr "CM"))
        (t (setf rv rs))
       )
       (if (null sposv) 
           rv
           ;otherwise
           ((lambda (spos subst) 
             (cond 
               ((eq spos 0)  
                 (setf rv (format nil "~A~A" subst (subseq rs 3)))
                )
               (t 
                 (setf rv (format nil "~A~A~A" (subseq rs 0 sposv) subst (subseq rs (+ sposv 3) )))
               )
             ) 
             (cleanup rv)) sposv substr )            
           )       
       ))

(defun convert-to-roman (number)
  (let ((counts ()) (adjust ()) (res ()) (fin ""))
    (setf counts (get-counts number))
    (multiple-value-bind (a r) (get-adjust counts () ())
      (format t "~A~%" a)
      (format t "~A~%" r)
      (setf res (render a r '(#\M #\D #\C #\L #\X #\V #\I) ()))
      (setf fin (coerce res 'string))
      (cleanup fin)      
      )))
      
       
(defun romanize (number)
  "Returns the Roman numeral representation for a given number."
  (if (or (> number 3999) (< number 1))
      nil
      (convert-to-roman number))
  )
