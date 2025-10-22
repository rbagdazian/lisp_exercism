 (defpackage :luhn
  (:use :cl)
  (:export :validp))

(in-package :luhn)




(defun validp (input)
  (if (<= (length input) 1) nil
     (let (
        (orgseq nil)
        (newseq nil)
        (revseq nil)
        (modval 0)
        )
       (setf orgseq (coerce input 'list))
       (setf orgseq (remove #\Space orgseq))
       (setf newseq (remove-if-not #'digit-char-p orgseq)) 
       (if (or (= (length orgseq) 1) (/= (length orgseq) (length newseq))) nil
           (progn
             (setf revseq (reverse newseq))
             (setf newseq (list))
             (loop for i from 0 to (1- (length revseq)) do
               (if (> (* 2 (digit-char-p (nth i revseq))) 9)
                   (setf modval (- (* 2 (digit-char-p (nth i revseq))) 9) )
                   (setf modval (* (digit-char-p (nth i revseq)) 2)) )
               (if (evenp i)
                 (setq newseq (append newseq (list (digit-char-p (nth i revseq)))))
                 (setq newseq (append newseq (list modval)))
                 ))
            (if (zerop (mod (reduce #'+ newseq) 10)) t
             nil)
             )
      ))
  ))
                                      
       
       
            
       
       
