#lang slideshow

(define (is-zero val)
  (cond
    [(= val 0) 1]
    [else 0]))


(define (count-trailing-zeros bits)
  (define (helper bits current)
  (cond
    [(= (first bits) 1) current]
    [else (helper (rest bits)
                  (+ current (is-zero (first bits))))]))
    (helper (reverse bits) 0))

(count-trailing-zeros '(1 0 0 1 1 0 1 0 0 0))