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

(define (remove-trailing-zeros bits)
  (define (helper bits n)
    (cond
      [(= n 0) (reverse bits)]
      [else (helper (rest bits) (- n 1))]))
  (helper (reverse bits) (count-trailing-zeros bits)))
; A function that returns the number of leading zeroes in a binary number given as a list of binary digits
(define (count-leading-zeros bits)
  (define (helper bits current)
  (cond
    [(= (first bits) 1) current]
    [else (helper (rest bits)
                  (+ current (is-zero (first bits))))]))
    (helper bits 0))

; A function that strips a binary number given as a list of binary digits of its leading zeros
(define (remove-leading-zeros bits)
  (define (helper bits n)
    (cond
      [(= n 0) bits]
      [else (helper (rest bits) (- n 1))]))
  (helper bits (count-leading-zeros bits)))

(define (decrement bits)
  (cond
    [(empty? (rest bits)) (list 0)]
    [else 
    (define (helper num-trailing-zeros bits)
      (define (add-ones-helper bits n)
        (cond
          [(= n 0) (reverse bits)]
          [else (add-ones-helper (append (list 1) bits) (- n 1))]))
      (add-ones-helper (append (list 0) (rest bits)) num-trailing-zeros)
      )
    (remove-leading-zeros (helper (count-trailing-zeros bits) (reverse (remove-trailing-zeros bits))))]
  ))

(decrement '(1 0 1 1 0))
(decrement '(1 0 0 0 0))
(decrement '(0))
