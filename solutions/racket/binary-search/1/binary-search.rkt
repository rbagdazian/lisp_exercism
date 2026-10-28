#lang racket

(provide binary-search)

(define (search-on arrays value li)
  (if (= (vector-length arrays) 0) 
      #f
  (let* ([mp (quotient (vector-length arrays) 2)]
         [mpv (vector-ref arrays mp)])
    (cond 
          ([and (= (vector-length arrays) 1) (not (= mpv value))] #f)
          ([= mpv value] (+ li mp))
          ([< mpv value] (search-on (vector-drop arrays mp) value (+ li mp)))
          ([> mpv value] (search-on (vector-drop-right arrays mp)value li))
          ([(<= (vector-length arrays) 1) #f]))
    ))
  )
    
    

(define (binary-search array value)
  (define arrays (vector-sort array <))
  (search-on arrays value 0))
