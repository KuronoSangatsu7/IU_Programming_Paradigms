#lang slideshow

; A function that returns the number of ones in a given list of binary digits
(define (count-ones bits)
  (define (helper bits current)
  (cond
    [(empty? bits) current]
    [else (helper (rest bits)
                  (+ current (first bits)))]))
    (helper bits 0))

(count-ones '(1 0 0 0 1 1 0 1 0 1))

; A function that returns the number of trailing zeroes in a binary number given as a list of binary digits
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

; A function that increments a binary number given as a list of binary digits
(define (increment bits)
  (define (helper bits has-one current)
  (cond
    [(= has-one 1) 
                    (cond
                      [(empty? bits) (reverse (append current '(1)))]
                      [else (cond
                            [(= (first bits) 1) (helper (rest bits) has-one (append current '(0)))]
                            [else (helper (rest bits) (- has-one 1) (append current '(1)))])
                          ])]
    [else
                    (cond
                      [(empty? bits) (reverse current)]
                      [else (helper (rest bits) has-one (append current (list(first bits))))])
                    ]))
    (helper (reverse bits) 1 empty))

(increment '(1 0 1 1 1 0 1 1))

; A function that converts a given decimal number to a list of binary digits
(define (decimal-to-binary n)
  (define (helper current-list n)
    (cond
      [(< n 2) (reverse (append current-list (list n)))]
      [else (helper (append current-list (list (remainder n 2))) (quotient n 2))])
    )
  (helper empty n))

(decimal-to-binary 5)

; A functiont that converts a binary number given as a list of binary digits to a decimal number
(define (binary-to-decimal bits)
  (define (helper bits res pow)
    (cond
      [(empty? bits) res]
      [else (helper (rest bits) (+ res (* (first bits) (expt 2 pow))) (+ 1 pow))]))
  (helper (reverse bits) 0 0))

(binary-to-decimal '(1 0 1 1 0)) 