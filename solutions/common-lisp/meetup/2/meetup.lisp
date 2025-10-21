(defpackage :meetup
  (:use :cl)
  (:export :meetup))

(in-package :meetup)


(defparameter *months* '(
                         (1 . "Jan")
                         (2 . "Feb")
                         (3 . "Mar")
                         (4 . "Apr")
                         (5 . "May")
                         (6 . "Jun")
                         (7 . "Jul")
                         (8 . "Aug")
                         (9 . "Sep")
                         (10 . "Oct")
                         (11 . "Nov")
                         (12 . "Dec")))


(defparameter *mokey* '(
                        ("Jan" . 1)
                        ("Jan_l" . 0)
                        ("Feb" . 4)
                        ("Feb_l" . 3)
                        ("Mar" . 4)
                        ("Apr" . 0)
                        ("May" . 2)
                        ("Jun" . 5)
                        ("Jul" . 0)
                        ("Aug" . 3)
                        ("Sep" . 6)
                        ("Oct" . 1)
                        ("Nov" . 4)
                        ("Dec" . 6)))

(defparameter *daykey* '(
                         (1 . :sunday)
                         (2 . :monday)
                         (3 . :tuesday)
                         (4 . :wednesday)
                         (5 . :thursday)
                         (6 . :friday)
                         (0 . :saturday)))

(defun leapyearp (year)
     (and (zerop (rem year 4)) (not (= (mod year 100) 0)))  
  )

(defun get-day-of-week (month day year)
  " this is a method to obtain day of week for a given year taken from the Farmers Almanac!"
  (let* ((yr100 (mod year 100)) 
         (yr-rem (floor (* yr100 1.25)))         
         (lp "")
         (key 0)
         )
    (setf yr-rem (+ yr-rem day))
    (if (and (leapyearp year) (or (string-equal month "Jan") (string-equal month "Feb")) ) 
        ;then
        (setf lp "_l"))

    ; get the correct month key
    (setf key (eval `(cdr (assoc (concatenate 'string ,month ,lp) *mokey* :test #'string-equal))))
    (cond
      ((< year 1753) (setf yr-rem nil))
      ((< year 1900) (setf yr-rem (+ yr-rem 2)))
      ((and (<= year 2099) (>= year 2000)) (setf yr-rem (1- yr-rem)))
      ( t t)
      )
    (if yr-rem 
        (setf yr-rem (rem (+ yr-rem key) 7))
        nil))
  )

(defparameter *days* '(
                       (:first . (1 2 3 4 5 6 7))
                       (:second . (8 9 10 11 12 13 14))
                       (:third . (15 16 17 18 19 20 21))
                       (:teenth . (13 14 15 16 17 18 19))
                       (:fourth . (22 23 24 25 26 27 28))
                       (:last . (25 26 27 28 29 30 31))))

(defun meetup (month year dayofweek week)
  "given month year dayofweek and week, find year month day
  We'll start by finding the first day of the month for the given month and year"
  (let ((monthn "") (fdom 0) (daylist nil) (weekpos 0) (dom 0) (lpyr nil))
    (setf monthn (cdr (assoc month *months*)))
    ;(format t "monthn: ~A ~%" monthn)
    (setf fdom (get-day-of-week monthn 1 year))
    ;(format t "Day of week: ~A ~A ~A ~%" fdom dayofweek week)
    (list fdom)
    (setf daylist  (append daylist  (mapcar #'(lambda (d) (get-day-of-week monthn d year)) (cdr (assoc week *days*)))))
    ;(format t "daylist: ~A ~%" daylist)
    (setf weekpos (position dayofweek  (mapcar #'(lambda (d) (cdr (assoc d *daykey*))) daylist)))
    (setf lpyr (and (zerop (rem year 4)) (not (= (mod year 100) 0 ))))
    ;(format t "weekpos: ~A leap?:~A ~%" weekpos lpyr )
    (setf dom (nth weekpos  (cdr (assoc week *days*))))
    (if (and (= month 2) (= dom 29) (not lpyr))
        (list year month (- dom 7))        
        (if (and (= month 2) (> dom 29))
          (list year month (- dom 7))
          (list year month dom)))
    ))

