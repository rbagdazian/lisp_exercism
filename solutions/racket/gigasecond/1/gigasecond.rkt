#lang racket

(provide add-gigasecond)
(require racket/date)

(define (add-gigasecond datetime)
  (let ((s (date->seconds datetime)))
    (seconds->date (+ s 1000000000))))

