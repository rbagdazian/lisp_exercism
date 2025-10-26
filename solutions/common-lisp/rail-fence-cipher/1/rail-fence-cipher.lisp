(defpackage :rail-fence-cipher
  (:use :cl)
  (:export :encode
           :decode))

(in-package :rail-fence-cipher)

(defun encode (msg rails )
  (let (
        (rlist (make-list rails))
        (ridx 0)
        (rdir :up)
        (res ""))
   (loop for ch across msg do
        (when (< ridx 0)
            (setf ridx 1)
            (setf rdir :up))
        (when (>= ridx rails)
            (setf ridx (- rails 2))
            (setf rdir :down))
        (setf (nth ridx rlist) (append (nth ridx rlist) (list ch)))
        (if (eq rdir :up)
            (setf ridx (1+ ridx))
            (setf ridx (1- ridx))
            )
         )
    (format t "~A~%~%" rlist)
    (loop for i from 0 to (1- rails) do
      (setf res (concatenate 'string res (coerce (nth i rlist) 'string))))
    res
    ))     


      
        

(defun decode (msg rails)
  (let (
        (rlist (make-list rails))
        (ridx 0)
        (cidx 0)
        (rdir :up)
        (res "")
        (currow nil)
        (outstr ""))
   (loop for i from 0 to (1- rails) do
         (setf (nth i rlist) (make-list (length msg))))
   (loop for i from 0 to (1- (length msg)) do
        (when (< ridx 0)
            (setf ridx 1)
            (setf rdir :up))
        (when (>= ridx rails)
            (setf ridx (- rails 2))
            (setf rdir :down))
        (setf currow (nth ridx rlist))
        (setf (nth i currow) t)
        (setf (nth ridx rlist) currow)
        (if (eq rdir :up)
            (setf ridx (1+ ridx))
            (setf ridx (1- ridx))
            )
         )
    ;(format t "~A~%~%" rlist)
    (setf ridx 0)
    (setf cidx 0)
    (loop for i from 0 to (1- rails) do
         (setf currow (nth i rlist))
         (loop for j from 0 to (1- (length msg)) do
              (when (nth j currow) 
                (setf (nth j currow) (char msg cidx))
                (setf cidx (1+ cidx)))
              )
         (setf (nth i rlist) currow)
          )
       (format t "~A~%" rlist)
    ; ok now reassemble the message
   (loop for i from 0 to (1- (length msg)) do
        (when (< ridx 0)
            (setf ridx 1)
            (setf rdir :up))
        (when (>= ridx rails)
            (setf ridx (- rails 2))
            (setf rdir :down))
        (setf currow (nth ridx rlist))
        (setf outstr (concatenate 'string outstr (string (nth i currow))))
        (if (eq rdir :up)
            (setf ridx (1+ ridx))
            (setf ridx (1- ridx))
            ))
       (format t "~A~%" outstr)
       outstr
        ))
        
