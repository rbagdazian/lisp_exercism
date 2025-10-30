(defpackage :spiral-matrix
  (:use :cl)
  (:export :spiral-matrix))

(in-package :spiral-matrix)

(defun spiral-matrix (size)
  (cond ((< size 1) nil)
  (t (let ((pos '(0 . 0))
        (dir :east)
        (step 1)
        (steps (* size size))
        (arr (make-array (list size  size)))
        )
    (loop while (<= step steps)
          do (print step)
          do (setf (aref arr (car pos) (cdr pos)) step)
          do (setf step (1+ step))          
          do (multiple-value-bind (npos ndir) (adjust-pos arr pos dir size)
               (setf pos npos) (setf dir ndir))                    
          )
    arr
    ))))

(defun adjust-pos (arr pos dir size )
      (case dir
        ((:east)
         (if  (or  (= (cdr pos) (1- size)) (/= (aref arr (car pos) (1+  (cdr pos))) 0))
              (progn
                (setf dir :south)
                (setf pos (cons (1+ (car pos))    (cdr pos)))
                )
              (setf pos (cons (car pos) (1+  (cdr pos))))))        
        ((:south)
         (if  (or  (= (car pos) (1- size)) (/= (aref arr (1+  (car pos)) (cdr pos)) 0))
              (progn
                (setf dir :west)
                (setf pos (cons (car pos) (1- (cdr pos))))
                )
              (setf pos (cons (1+  (car pos)) (cdr pos)))))
        ((:west)
         (if  (or  (= (cdr pos) 0) (/= (aref arr  (car pos) (1-  (cdr pos))) 0))
              (progn
                (setf dir :north)                               
                (setf pos (cons (1-  (car pos)) (cdr pos))))
              (setf pos (cons (car pos) (1- (cdr pos))))
              ))
        ((:north)
         (if  (/= (aref arr  (1-  (car pos))  (cdr pos))  0)
              (progn
                (setf dir :east)
                (setf pos (cons  (car pos)  (1+ (cdr pos))))
                )
              (setf pos (cons (1- (car pos))  (cdr pos)))))        
        )
      (values pos dir)                                 
  )

