#lang racket
(require racket/format)
(require rackunit rackunit/text-ui)
(provide clock
         add
         subtract
         clock->string)

(define clock-val #(0 0))

(define (clock->string c)
  (format "~a:~a" (~r (list-ref c 0) #:min-width 2 #:pad-string "0") (~r (list-ref c 1) #:min-width 2 #:pad-string "0"))
  )

(define (clock m s)
  (let* ([sc (quotient s 60)]
        [sn (remainder s 60)]
        [mn (remainder (+ (remainder m 24) sc) 24)]
        [clock-val (list 0 0)]
        )
    (cond ((< sn 0) (set! sn (+ sn 60)) (set! mn (- mn 1)) (set! clock-val (list mn sn)))
          (#t (set! clock-val (list mn sn)))
          )
    (cond ((< mn 0) (set! mn (+ mn 24)) (set! clock-val (list mn sn)))
          (#t #t))
    clock-val
    )
  )
  

(define (add c minutes)
  (set! clock-val (clock (list-ref c 0) (+ (list-ref c 1) minutes)))
  clock-val)

(define (subtract c minutes)
  (set! clock-val (clock (list-ref c 0) (- (list-ref c 1) minutes)))
  clock-val)