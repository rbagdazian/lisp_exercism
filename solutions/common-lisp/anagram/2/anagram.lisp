(defpackage :anagram
  (:use :cl)
  (:export :anagrams-for))

(in-package :anagram)

;; this function considers each candidate in succession and if it is an anagram
;; it is appended to the acc variable and then calls itself recursively to inspect
;; the next word in the candidates list, until all candidates have been examined.

(defun collect-anagrams (subject candidates acc)
  (if (null candidates) acc
  ;otherwise
  (let* ((lowsubj (string-downcase (copy-seq subject) ) )
        (lowcand (string-downcase (copy-seq (car candidates))))
        (sortsubj (sort (copy-seq lowsubj) #'char-lessp))
        (sortcand (sort (copy-seq lowcand) #'char-lessp)))
        (cond 
        ((string-equal lowsubj  lowcand)                          ; reject same word
            (collect-anagrams subject (rest candidates) acc))     ; continue evaulating rest of candidates
        ((string-equal sortsubj sortcand)                         ; check for true anagram
              (setf acc (append acc (list (first candidates))))   ; if so append to accumulation list
              (collect-anagrams subject (rest candidates) acc))   ; and call self recursivly to continue
        (t (collect-anagrams subject (rest candidates) acc)))    ; here if candidate not anagram.
    )))

;; this function calls the worker function that is used to collect the 
;; list of anagrams found.
(defun anagrams-for (subject candidates)
  "Returns a sublist of candidates which are anagrams of the subject."
     (collect-anagrams subject candidates ())
    )


