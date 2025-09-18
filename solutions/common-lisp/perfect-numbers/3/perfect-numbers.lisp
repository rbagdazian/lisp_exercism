(defpackage :perfect-numbers
  (:use :cl)
  (:export :classify))

(in-package :perfect-numbers)

(defun get-aliquot-sum (n)
  (let ((aliquot-sum 1) 
        (x 2) 
        (max-x (floor (sqrt n))) 
        (q 0) 
        (ok t))
    (if (eql n 2) 
      ;then
      (setf aliquot-sum 1)
      ;else
      (loop
       :with quot
       :with rem
       :for x :from 2 :to max-x :do
         (setf (values quot rem) (floor n x))
         (when (zerop rem)
           (incf aliquot-sum (+ x quot)))
        :finally
          (when (= (* max-x max-x) n)
            (decf aliquot-sum max-x))
       )
      )
      aliquot-sum 
    ))

(defun get-low-factors (n m acc)
   (cond ((null m) (get-low-factors n (nth-value 0 (floor (sqrt n))) acc))
     ((= m 1) (reverse (append acc (list 1))))
     ((integerp (/ n m)) (let ((nl (append acc (list m)))) 
                              (get-low-factors n (- m 1) nl)))
     (T (get-low-factors n (- m 1) acc))))

(defun get-high-factors(n acc hiacc)
  (cond ((null acc) hiacc)
        (T (get-high-factors n (cdr acc) (append hiacc (list (/ n (car acc))))))))

(defun get-all-factors (n)
  (let ((lf 0) (hf 0))
    (setq lf (get-low-factors n nil nil))
    (setq hf (get-high-factors n (cdr lf) nil))
    (cond ((= (length lf) 1) lf)
          ((= (car (last lf)) (car hf)) lf)
          (t (append lf (reverse hf))))
   ))

(defun classify_old (number)
  (let ((factors 0))
  (cond ((< number 1) nil)
        ((= number 1) "deficient")
        (t (setq factors (get-all-factors number))
           (cond
             ((= (length factors) 1) "deficient")
             ((= (reduce #'+ factors) number) "perfect")
             ((> (reduce #'+ factors) number) "abundant")
             ((< (reduce #'+ factors) number) "deficient")
            (t (list 'unknown' factors)))))))

(defun classify (number)
  (cond ((< number 1) nil)
        ((= number 1) "deficient")
        (t (let ((sumv (get-aliquot-sum number)))
             (cond
               ((null sumv) nil)
               ((= sumv number) "perfect")  
               ((> sumv number) "abundant")
               ((< sumv number) "deficient")               
               )))))
