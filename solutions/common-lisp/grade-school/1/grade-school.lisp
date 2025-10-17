(defpackage :grade-school
  (:use :cl)
  (:export :make-school :add :roster :grade))

(in-package :grade-school)

;;;; Welcome to Portacle, the Portable Common Lisp Environment.
;; For information on Portacle and how to use it, please read
;;   https://portacle.github.io or *portacle-help*
;; To report problems and to get help with issues,  please visit
;;   https://github.com/portacle/portacle/issues
;; 
;; You can use this buffer for notes and tinkering with code.

(defparameter *hash-table* (make-hash-table :test 'equal))

(defun make-school ()
  (clrhash *hash-table*)
  *hash-table*)

(defun add (school student grade )
  (if (gethash student school) nil
      (setf (gethash student school) grade)
      )
  )

(defun get-hash-table-keys (hash-table)
  (let ((keys ()) (vals ()) (alist ()) )
    (maphash (lambda (k v) (push k keys) (push v vals) (push (cons k v) alist)) hash-table)
    (values keys vals alist)))

(defun get-hash-table-values-that-match (hash-table m)
  (let ((keys ()))
    (maphash (lambda (k v) (if (eql v m) (push k keys))) hash-table)
    keys))

(defun sort-names-by-grade (al)
  " this function sorts by grade level, then
  sorts each group of names in the same level
  Input is an association list of k v pairs
  which contain the name and grade.
  First we sort on grade, then on names within
  the individual grades."
  (let ((sal nil) (cel nil) (cname nil) (pgrade 0) (cgrade 0) (tmpnLst '()) (tmpgLst '()) (out-list '()) (grade-list '()))
    (setq sal
          (sort (copy-list al)
                (lambda (x y) (< (cdr x) (cdr y)))))
    ;(format t "Sorted on grades: ~A~%" sal)
    (do* ((worklist (copy-list sal) (cdr worklist)))
         ((null worklist) out-list)
      (setf cel (car worklist))
      ;(format t "cel: ~A tmpnLst: ~A tmpgLst: ~A pgrade: ~A~%" cel tmpnLst tmpgLst pgrade)
      (setf cname (car cel))
      (setf cgrade (cdr cel))
      (when (and (/= cgrade pgrade) (/= pgrade 0))
        ; save current list to out list
        (setq tmpnLst (sort (copy-list tmpnLst) #'string<))
        ;(format t "new tmpnLst: ~A ~%" tmpnLst)
        ;(format t "new tmpgLst: ~A ~%" tmpgLst)
        (setf out-list (nconc out-list tmpnLst))
        (setf grade-list (nconc grade-list tmpgLst))
        (setf tmpnLst '())
        (setf tmpgLst '())
        )
      (setf tmpnLst (nconc tmpnLst (list  cname)))
      (setf tmpgLst (nconc tmpgLst (list cgrade)))
      (setf pgrade cgrade)
      )
    ;(format t "last element: ~A ~A tmpnLst: ~A tmpgLst: ~A ~%" cname cgrade tmpnLst tmpgLst)
    (setq tmpnLst (sort (copy-list tmpnLst) #'string<))
    (setf out-list (nconc out-list tmpnLst))
    (setf grade-list (nconc grade-list tmpgLst))
    (format t "final sort: ~A ~%" out-list)
    (format t "final glist: ~A ~%" grade-list)
    (values  out-list grade-list)
    )
  )


(defun roster (school)
  (if (= (hash-table-count school) 0)
      nil  
      (multiple-value-bind (k v al)
          (get-hash-table-keys school)
        (let ((sorted-list '()) (sorted-names '()) (names nil))
          (setq sorted-list (sort-names-by-grade al))
          (format t "roster: ~A ~%" sorted-list)
          sorted-list          
          ))
      ))

(defun grade (school level)
  (if (= (hash-table-count school) 0)
      nil  
      (sort (get-hash-table-values-that-match school level) #'string<)
      ))
