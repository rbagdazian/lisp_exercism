(defpackage :anagram
  (:use :cl)
  (:export :anagrams-for))

(in-package :anagram)

;; utility to remove first occurrence of char from string
;; and return that result.
(defun remove-first-char (char string)
  (let ((pos (position char string :test #'char-equal)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     (subseq string (1+ pos)))
        string))) ; If char not found, return original string

;; this is a predicate that determines if the candidate string is an anagram of the subject string
;; it looks for the first character of subject occuring anywhere in the candidate string and
;; if found eliminates both characters and calls itself recursively.
;; if the function eventually is called with both subject and candidate empty strings, then we
;; have passed the test and return true.
(defun anagramp (subject candidate)  
  (cond ((and (eq (length subject) 0) (eq (length candidate) 0)) 
        ; if we get here the candidate is an anagram of the subject 
        ; since all characters of the candidate were consumed, so return true.
        t)
      ((eq (length subject) 0) nil) ; reject since subject is empty but candidate is not.
      ((eq (length candidate) 0) nil) ; reject since candidate is empty but subject is not.
      ((find (char subject 0) candidate :test #'char-equal) ; look for match in leading character
            ; eliminate leading character of subject and first matching character from candidate
            ; then call recursively
            (anagramp (subseq subject 1) (remove-first-char (char subject 0) candidate))
             )
      (t nil) ; leading character of subject was not found in candidate so reject. 
    ))

;; this function considers each candidate in succession and if it is an anagram
;; it is appended to the acc variable and then calls itself recursively to inspect
;; the next word in the candidates list, until all candidates have been examined.
;; if the first word in the candidates list is not an anagram, the function simply
;; calls itself recursively eliminating the leading candidate word.
(defun collect-anagrams (subject candidates acc)
  (cond ((null candidates) acc) ; return accumulated list if candidates is null set
        ((string-equal subject (first candidates)) ; reject same word
            (collect-anagrams subject (rest candidates) acc)) ; continue evaulating rest of candidates
        (t (cond 
             ((anagramp subject (first candidates)) ; check for first element in candidates an anagram
              (setf acc (append acc (list (first candidates))))   ; if so append to accumulation list
              (collect-anagrams subject (rest candidates) acc))     ; and call self recursivly to continue
             (t (collect-anagrams subject (rest candidates) acc)))) ; here if candidate not anagram.
    ))

;; this function calls the worker function that is used to collect the 
;; list of anagrams found.
(defun anagrams-for (subject candidates)
  "Returns a sublist of candidates which are anagrams of the subject."
     (collect-anagrams subject candidates ())
    )

