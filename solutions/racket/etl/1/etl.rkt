#lang racket

(provide etl)

(define (etl input)
  (cond ([= (hash-count input) 0] input)
        (#t
         (let ((nht (make-hash)))
           (hash-map input (λ (k v)
                             (if (<= k 0)
                                 (raise (exn:fail:contract "Negative key values not allowed" 
                                                           (current-continuation-marks)))
                                 (for ([i v]) (hash-set! nht  (string-downcase i) k)))
                             ))
           nht
           )
         )
        ))

