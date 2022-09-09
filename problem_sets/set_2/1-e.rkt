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

(define (count-leading-zeros bits)
  (define (helper bits current)
  (cond
    [(= (first bits) 1) current]
    [else (helper (rest bits)
                  (+ current (is-zero (first bits))))]))
    (helper bits 0))

(define (remove-leading-zeros bits)
  (define (helper bits n)
    (cond
      [(= n 0) bits]
      [else (helper (rest bits) (- n 1))]))
  (helper bits (count-leading-zeros bits)))

; Algorithm: To subtract 1 from a number (1 0 1 1 0 0) all we need to do is flip all the bits after the rightmost 1 bit
; to get (1 0 1 1 1 1) and then flip our previously rightmost 1 bit as well (1 0 1 0 1 1) then remove any leading zeros.
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

; That was one way to do decrement. After writing all that I realized that I could have just converted it 
; to decimal then done the operation then converted back to binary since I implemented the functions for conversion previously
; It was good practice nontheless :)

(define (decimal-to-binary n)
  (define (helper current-list n)
    (cond
      [(< n 2) (reverse (append current-list (list n)))]
      [else (helper (append current-list (list (remainder n 2))) (quotient n 2))])
    )
  (helper empty n))

(define (binary-to-decimal bits)
  (define (helper bits res pow)
    (cond
      [(empty? bits) res]
      [else (helper (rest bits) (+ res (* (first bits) (expt 2 pow))) (+ 1 pow))]))
  (helper (reverse bits) 0 0))

(define (easy-decrement bits)
  (cond
    [(= (binary-to-decimal bits) 0) (list 0)]
    [(= (binary-to-decimal bits) 1) (list 0)]
    [else (decimal-to-binary (- (binary-to-decimal bits) 1))])
  )

(easy-decrement '(1 0 1 1 0))
(easy-decrement '(1 0 0 0 0))
(easy-decrement '(0))