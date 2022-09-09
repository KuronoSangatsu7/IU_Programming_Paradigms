#lang slideshow
(define (count-ones bits)
  (define (helper bits current)
  (cond
    [(empty? bits) current]
    [else (helper (rest bits)
                  (+ current (first bits)))]))
    (helper bits 0))

(count-ones '(1 0 0 0 1 1 0 1 0 1))