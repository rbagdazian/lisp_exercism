(defpackage :pythagorean-triplet
  (:use :cl)
  (:export :triplets-with-sum))

(in-package :pythagorean-triplet)

(defun triplets-with-sum (n)
  (find-candidate-triplets n))

;;;; Welcome to Portacle, the Portable Common Lisp Environment.
;; For information on Portacle and how to use it, please read
;;   https://portacle.github.io or *portacle-help*
;; To report problems and to get help with issues,  please visit
;;   https://github.com/portacle/portacle/issues
;; 
;; You can use this buffer for notes and tinkering with code.

(defpackage :pythagorean-triplet
  (:use :cl)
  (:export :triplets-with-sum))

(in-package :pythagorean-triplet)

(defun triplets-with-sum (n)
  (find-candidate-triplets n))

;;;; Welcome to Portacle, the Portable Common Lisp Environment.
;; For information on Portacle and how to use it, please read
;;   https://portacle.github.io or *portacle-help*
;; To report problems and to get help with issues,  please visit
;;   https://github.com/portacle/portacle/issues
;; 
;; You can use this buffer for notes and tinkering with code.

(defun tri-length (phi)
  " this function returns the length of the 3 sides of a triangle
    for a given angle phi between 0 and 2pi"
  (let ((a (sin phi)) (b (cos phi)))
    (+ a b 1)
    )
  )

(defun get-k-value (N phi)
  (/ N (tri-length phi)))

(defun get-k-bounds (N start finish)
  (list (get-k-value N finish) (get-k-value N start)))


(defun get-vec-for-k (N start finish step)
  (loop for x from start to finish by step collect (list x (get-k-value N x) ) ))


(defun find-candidate-integers (klist lastint acc)
  (cond ((null klist) acc)
        ((< (ceiling (cadr (car klist))) lastint)
         (progn
           (setf acc (append acc (list (car klist))))
           (setf lastint (ceiling (cadr (car klist))))
           (find-candidate-integers (cdr klist) lastint acc)
           ))
        (t (find-candidate-integers (cdr klist) lastint acc) )
        ))


(defun check-triplets (tlist acc)
  (cond ((null tlist)  acc)
        (t       (let* (
                        (tl (car tlist))
                        (a (nth 0 tl))
                        (b (nth 1 tl))
                        (c (nth 2 tl))
                        )
                   (if (> a 150)    
                       (format t "a:~A b:~A c:~A \= ~A ~%" a b c (sqrt (+ (expt a 2) (expt b 2))) ))
                   (cond ((/= 0 (- (+ (expt a 2) (expt b 2)) (expt c 2)))
                          (check-triplets (cdr tlist) acc))
                         (t
                          (setf acc (append acc (list (car tlist))))
                          (check-triplets (cdr tlist) acc))
                         )           
                   ))))

(defun find-candidate-triplets ( N )
  
  (let* (
         (klist (get-vec-for-k N (/ pi 180.0) (* pi 0.25) (/ pi (* 4 N) )))
         (initv (cadr (car klist)))
         (kint (find-candidate-integers klist initv '()))
         (triplist '()))
    (format t "~A~%" klist)
    (loop for (phi k) in kint
          do (setf triplist 
                   (append triplist
                           (list  (list (round  (* (round k) (sin phi)) ) (round  (* (round k) (cos phi)))  (round k)) ))))
                                        ;(format t "~A ~%" triplist)
    (if (not (null triplist))
        (check-triplets triplist '())
        nil)
    )
  )
















