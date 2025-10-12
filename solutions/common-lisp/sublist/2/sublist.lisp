(defpackage :sublist
  (:use :cl)
  (:export :sublist))

(in-package :sublist) 

; using built in facilities
(defun sublist (list1 list2)
  (cond ((equalp list1 list2) :equal)
        ((search list1 list2) :sublist)
        ((search list2 list1) :superlist)
        (t :unequal)))

;
;; solution without using built in facilities
;
(defun sublist (list1 list2)
  (cond ((equalp list1 list2) :equal)
        ((search list1 list2) :sublist)
        ((search list2 list1) :superlist)
        (t :unequal)))

(defun look-for-match (short long)
    (cond ((null short) t)
          ((eql (car short) (car long))
           (look-for-match (cdr short) (cdr long)))
           (t nil)))


(defun my-sublist (list-one list-two)
  (let ((class nil))
       (cond ((< (length list-one) (length list-two))
              (setf class :sublist)
              (do ( (short list-one) 
                    (long list-two (cdr long)) 
                    (res :unequal))
                  ((or (> (length short) (length long)) (eql res class)) res)
                  (if (look-for-match short long) (setf res class))))
             ((> (length list-one) (length list-two))
              (setf class :superlist)
              (do ( (short list-two) 
                     (long list-one (cdr long)) 
                     (res :unequal))
                  ((or (> (length short) (length long)) (eql res class)) res)
                  (if (look-for-match short long) (setf res class))))      
              (t 
               (if (look-for-match list-one list-two) :equal :unequal)))))    

                  



  
    
          
     
