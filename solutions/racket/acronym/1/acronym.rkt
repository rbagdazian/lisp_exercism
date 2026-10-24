#lang racket

(provide acronym)

(define (acronym string)
  (cond [(= (string-length string) 0) ""]
        [#t (let* (
                    [toks (string-split string  #px"[\\s\\-\\_]+")]
                    [charvs ( map (lambda (x) (char-upcase (string-ref x 0))) toks)])
               (list->string charvs))])
  )