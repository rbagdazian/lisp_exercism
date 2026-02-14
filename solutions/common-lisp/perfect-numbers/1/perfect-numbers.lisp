(defpackage :perfect-numbers
  (:use :cl)
  (:export :classify))

(in-package :perfect-numbers)


(defun filtn (n acc)
  " this function removes all multiples of n"
  (let ((m (+ n n)) (lastv (car (last acc))))
    (loop while (<= m lastv)
          do (setq acc (delete m acc))
          do (setq m (+ m n))
          ) acc))
  
(defun sieve (n acc)
  " this function implements the sieve of eratosthenes"
  (cond ((null acc) (sieve 2 (loop for i from 1 to (/ n 2) collect i)))
        ((> n (car (last acc))) acc)
        (T (sieve (+ n 1) (filtn n acc)))))


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

(defun classify (number)
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


