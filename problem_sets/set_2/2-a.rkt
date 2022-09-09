#lang slideshow
(define (alternating-sum bits)
  (define (helper bits flag sum)
    (cond
      [(empty? bits) sum]
      [(= flag 0) (helper (rest bits) 1 (+ (first bits) sum))]
      [else (helper (rest bits) 0 (- sum (first bits)))]))
  (helper bits 0 0))
  
(alternating-sum (list 6 2 4 1 3 9))