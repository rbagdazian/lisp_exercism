#lang racket

(provide encode decode)

(define pt "abcdefghijklmnopqrstuvwxyz")

(define ct (list->string (reverse (string->list pt))))

(define (docode m tt)
  (define ns "")
  (define final "")
  (define sl (string->list (string-downcase m)))
  (for ([c sl])
    (cond (
           (and (char>=? c #\a) (char<=? c #\z))           
           (let* ([idx (- (char->integer c) (char->integer #\a))]
                  [ctt (string-ref ct idx)])
             (set! ns (string-append ns (string ctt)))
             ))
          ((and (char>=? c #\0) (char<=? c #\9))
           (set! ns (string-append ns (string c))))
          )
    " now if ns is of length 5 emit it to the final output string"
    (cond ((= (string-length ns) 5)
           (set! final (string-append final ns))
           (if (= tt 1)
               (set! final (string-append final " "))
               final
               )
           (set! ns ""))
          (#t #t)
        )
    final
    )
  (set! final (string-append final ns))
  (string-trim final)
)  

(define (encode m)
  (docode m 1)
  )
              
(define (decode m)
  (docode m 2)
  )

