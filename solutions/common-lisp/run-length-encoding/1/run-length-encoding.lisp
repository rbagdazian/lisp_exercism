(defpackage :run-length-encoding
  (:use :cl)
  (:export :encode
           :decode))

(in-package :run-length-encoding)

(defun encode (plain &aux (rchar #\!) (rcount 0) (outstr "") )
  (if (> (length plain) 0)
    (loop for ch across plain do
          (format t "~a.~%" ch)
          (cond ((char/= ch rchar) 
                 (cond ((> rcount 1) 
                       (format t "rcount1: ~A~%" rcount)
                       (setf outstr (format nil "~A~A~A" outstr (write-to-string rcount) rchar ))
                       (setf rcount 1) (setf rchar ch) (format t "outstr(1): ~A~%" outstr))
                       ((= rcount 1)
                       (format t "rcount2: ~A~%" rcount)
                       (setf outstr (format nil "~A~A" outstr rchar ))
                       (setf rchar ch) (format t "outstr(2): ~A~%" outstr))
                       ((zerop rcount)
                       (setf rchar ch) (setf rcount 1))))                                
                (t (setf rcount (1+ rcount))))
    ))  
    (cond ((zerop (length plain)) (setf outstr ""))
    (t
      (cond ((> rcount 1) 
       (setf outstr (format nil "~A~A~A" outstr (write-to-string rcount) rchar )))
            ((= rcount 1) 
       (setf outstr (format nil "~A~A" outstr rchar )))
     )))
  outstr
  )
      

  

(defun decode (compressed &aux (state :start) (rcount 0) (outstr ""))
    (loop for ch across compressed do
      (cond ((eql state :start)
             (if (setf rcount (digit-char-p ch))
                 (setf state :get-count)
                 (setf outstr (format nil "~A~A" outstr ch)))
             (format t ":start ~A~%" outstr)
             )
             ((eql state :get-count)
              (if (digit-char-p ch) 
                  (setf rcount (+ (* rcount 10) (digit-char-p ch)))
                  (progn
                    (setf outstr (format nil "~a~a" outstr (make-string rcount :initial-element ch)))
                    (setf state :start)))
             (format t ":get-count ~A~%" outstr)              
             )
             ))
     outstr
  )
                          

