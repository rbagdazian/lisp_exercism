#lang racket

(provide (struct-out character)
         ability
         make-character
         modifier
)

(struct character
    (strength dexterity constitution intelligence wisdom charisma modifier hitpoints) #:mutable #:transparent) ; Add necessary struct fields

(define (ability)
  (get-power-level))

(define (modifier value)
  (inexact->exact (floor (/ (- value 10) 2))))

(define (dice-throw) 
  (let ((d1 (random 1 6))
        (d2 (random 1 6))
        (d3 (random 1 6))
        (d4 (random 1 6))
        )
    (vector  d1 d2 d3 d4)
    )
  )


(define (vector-min v)
  (let ((minv 1000000))
    (for ([x (in-vector v)])
      (if (< x minv) (set! minv x) #f ) )
    minv)
  )

(define (vector-remove-val v m)
  (let ((done 0) (nv #()))
    (for ([x (in-vector v)])
      (if (and (= x m) (= done 0)) (set! done 1) (set! nv (vector-append nv (vector x)) ))
      )
    nv
    ))

(define (vector-sum v acc)
  (cond ((= (vector-length v) 0) acc)
        (#t (vector-sum (vector-drop v 1) (+ acc (vector-ref v 0))))))

(define (get-power-level)
  (let ([dt (dice-throw)])
    (set! dt (vector-remove-val dt (vector-min dt)))
    (vector-sum dt 0)
    ))

(define (make-character)
  (let* (( mychar (character 0 0 0 0 0 0 0 0)))
    (set-character-strength! mychar (get-power-level))
    (set-character-dexterity! mychar (get-power-level))
    (set-character-constitution! mychar (get-power-level))
    (set-character-intelligence! mychar (get-power-level))
    (set-character-wisdom! mychar (get-power-level))
    (set-character-charisma! mychar (get-power-level))
    (set-character-modifier! mychar (modifier (character-constitution mychar)))
    (set-character-hitpoints! mychar (+ (character-modifier mychar) 10))
    mychar
    ))




