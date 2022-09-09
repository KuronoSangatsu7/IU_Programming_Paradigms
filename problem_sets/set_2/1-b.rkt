#lang slideshow

; Checking whether a given value is zero
(define (is-zero val)
  (cond
    [(= val 0) 1]
    [else 0]))

; Counting the number of leading zeros
(define (count-leading-zeros bits)
  (define (helper bits current)
  (cond
    [(= (first bits) 1) current]
    [else (helper (rest bits)
                  (+ current (is-zero (first bits))))]))
    (helper bits 0))

;(count-leading-zeros '(0 1 0 0 1 1 0 1 0 0 0))

; Counting the number of zeros
(define (count-zeros bits)
  (define (helper bits current)
  (cond
    [(empty? bits) current]
    [else (helper (rest bits)
                  (+ current (is-zero (first bits))))]))
    (helper bits 0))

;(count-zeros '(1 0 0 1 1 1 0 1))

(define (count-non-leading-zeros bits) 
    (- (count-zeros bits) (count-leading-zeros bits)))

(count-non-leading-zeros '(0 0 0 1 0 1 1 0))
