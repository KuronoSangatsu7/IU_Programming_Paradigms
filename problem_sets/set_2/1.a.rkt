#lang slideshow
(define (binary-to-decimal bits)
  (define (helper bits res pow)
    (cond
      [(empty? bits) res]
      [else (helper (rest bits) (+ res (* (first bits) (expt 2 pow))) (+ 1 pow))]))
  (helper (reverse bits) 0 0))

(binary-to-decimal '(1 0 1 1 0)) 