#lang slideshow

; Checking whether a given value is equal to one
(define (is-one val)
  (cond
    [(= val 1) #t]
    [else #f]))

; Checking parity of the given number 
(define (binary-odd bits)
  (define (helper bits)
    (is-one (first bits)))
  (helper (reverse bits)))

(binary-odd '(1 0 1 1 0))
(binary-odd '(1 0 1 1 1))