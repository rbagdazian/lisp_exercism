(defpackage :two-bucket
  (:use :cl)
  (:export :measure))

(in-package :two-bucket)


  (defun make-state (b1 b2)
  (cons b1 b2))

(defun state-b1 (state)
  (car state))

(defun state-b2 (state)
  (cdr state))

(defun is-valid (state start-bucket cap1 cap2)
  (let ((b1 (state-b1 state))
        (b2 (state-b2 state)))
    (cond
      ((and (eql start-bucket :one) (= b1 0) (= b2 cap2)) nil)
      ((and (eql start-bucket :two) (= b2 0) (= b1 cap1)) nil)
      (t t))))

(defun get-next-states (state cap1 cap2)
  (let ((b1 (state-b1 state))
        (b2 (state-b2 state))
        (res nil) )
    (setf res  (list
                (make-state cap1 b2)            ; Fill B1
                (make-state b1 cap2)            ; Fill B2
                (make-state 0 b2)               ; Empty B1
                (make-state b1 0)               ; Empty B2
                ;; Pour B1 → B2
                (let ((pour (min b1 (- cap2 b2))))
                  (make-state (- b1 pour) (+ b2 pour)))
                ;; Pour B2 → B1
                (let ((pour (min b2 (- cap1 b1))))
                  (make-state (+ b1 pour) (- b2 pour)))))
    ;(format t "next states: ~A ~%" res)
    res
    ))

(defun measure (bucket-one bucket-two goal start-bucket)
  "Function to solve the two-bucket puzzle, if possible, when given the capacities
of both buckets, a goal, and which bucket to start with.  Returns an alist of moves
required to reach the goal, the name of the bucket that reach the goal, and the
amount of water left over in the other bucket."
  (cond ((and (= bucket-two goal) (eql start-bucket :one))
         `((:moves . 2) (:goal-bucket . :two) (:other-bucket . ,bucket-one)))
        ((and (= bucket-one goal) (eql start-bucket :two))
         `((:moves . 2) (:goal-bucket . :one) (:other-bucket . ,bucket-two)))
    (t
  (let ((visited (make-hash-table :test 'equal))
        (queue '())
        (solution nil)
        (goal-bucket nil)
        (other-bucket nil))
    ;; Initial state
    (setf (gethash (make-state 0 0) visited) t)

    ;; First moves
    (let ((first-moves
            (cond
              ((eql start-bucket :one) (list (make-state bucket-one 0)))
              ((eql start-bucket :two) (list (make-state 0 bucket-two)))
              (t '()))))
      (dolist (state first-moves)
        (when (is-valid state start-bucket bucket-one bucket-two)
          (push (cons state (list state)) queue)
          (setf (gethash state visited) t)
          (format t "state: ~a~%" state)
          (format t "~A~%" queue)
          (format t "~A~%" (gethash state visited))
          (let (alist)
            (maphash #'(lambda (key value)
                         (push (cons key value) alist)) visited)
            ;(format t "visited: ~A~%" (nreverse alist))
               )
          (format t "~%")
          )
        ))

    ;; BFS loop
    (loop while queue do
          ;(format t "top of bfs loop queue: ~A~%" queue)
          (let* ((entry (pop queue))
                 (state (car entry))
                 (history (cdr entry))
                 (b1 (state-b1 state))
                 (b2 (state-b2 state))                 
                 )
            ;(format t "after let*: b1: ~a  b2: ~a ~%" b1 b2)
            (when (or (= b1 goal) (= b2 goal))
              (setf solution (reverse history))
              (format t "reached goal: ~A~% steps: ~A~%" solution (length solution))                         (when (= b1 goal)
                (setf goal-bucket :one)
                (setf other-bucket b2))                                                                      (when (= b2 goal)
                (setf goal-bucket :two)
                (setf other-bucket b1))                                     
              (return))
            (dolist (next (get-next-states state bucket-one bucket-two))
              (when (and (not (gethash next visited))
                         (is-valid next start-bucket bucket-one bucket-two))
                (format t "V: ~a~%" next)
                (setf (gethash next visited) t)
                (push (cons next (cons next history)) queue)))))
    ;(format t "solution: ~A~%" solution)
    (if solution
    (list (cons :moves (length solution))
          (cons :goal-bucket goal-bucket)
          (cons :other-bucket other-bucket))          
    nil)
    )))) ; No solution




