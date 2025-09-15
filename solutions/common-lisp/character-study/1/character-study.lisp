(defpackage :character-study
  (:use :cl)
  (:export
   :compare-chars
   :size-of-char
   :change-size-of-char
   :type-of-char))
(in-package :character-study)

(defun compare-chars (char1 char2)
  (cond ((char= char1 char2) :equal-to)
        ((char< char1 char2) :less-than)
        (T :greater-than)))

(defun size-of-char (char)
  (cond ((not(alphanumericp char)) :no-size)
        ((upper-case-p char) :big)
         (T :small)))

(defun change-size-of-char (char wanted-size)
 (cond ((not(alphanumericp char)) char)
        ((equal wanted-size :small) (char-downcase char))
         ((equal wanted-size :big) (char-upcase char))
         (T char)))
  
(defun type-of-char (char)
  (cond ((alpha-char-p char) :alpha)
        ((digit-char-p char) :numeric)
        ((char= #\Space char) :space)
        ((char= #\Newline char) :newline)
        (T :unknown)))
  

