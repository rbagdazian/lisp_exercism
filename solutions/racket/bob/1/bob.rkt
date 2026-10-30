#lang racket

(provide response-for)

(define (response-for prompt)
  (cond
    ([regexp-match #rx"^[A-Za-z0-9:!\n\r\\)\\., ]+[?][ ]*$" prompt] "Sure.")    
    ([regexp-match #rx"^[A-Z0-9' ]+[?]$" prompt] "Calm down, I know what I'm doing!")
    ([regexp-match #rx"^[\t\n\r? ]*$" prompt] "Fine. Be that way!")
    ([regexp-match #rx"^[0-9, ]+$" prompt] "Whatever.")    
    ([regexp-match #rx"^[A-Z0-9!,?@\\%\\#\\$\\(\\*\\^ ]+[!\\.]*$" prompt] "Whoa, chill out!")    
   
    (#t "Whatever.")
    ))
