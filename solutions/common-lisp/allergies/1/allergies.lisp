(defpackage :allergies
  (:use :cl)
  (:shadow :list)
  (:export :allergic-to-p :list))

(in-package :allergies)

(defvar *allergen-scores*
       (cl:list (cons "eggs" 1)
        (cons "peanuts" 2)
        (cons "shellfish" 4)
        (cons "strawberries" 8)
        (cons "tomatoes" 16)
        (cons "chocolate" 32)
        (cons "pollen" 64)
        (cons "cats" 128))  
  )
 

(defun allergic-to-p (score allergen)
  "Returns true if given allergy score includes given allergen."
  (let (
       (allergen-score (cdr (assoc allergen *allergen-scores* :test #'string-equal))))
       (if (zerop (logand allergen-score score)) nil
           (cdr (assoc allergen *allergen-scores* :test #'string-equal)) 
        )
  )
)

(defun list (score)
  "Returns a list of allergens for a given allergy score."
  (do ((citem 1 (* citem 2))
       (allergen-list '() allergen-list))
      ((> citem 128) allergen-list)
     (if (> (logand citem score) 0) (setf allergen-list (nconc allergen-list  (cl:list (car (rassoc  citem *allergen-scores*))))))
  ))
