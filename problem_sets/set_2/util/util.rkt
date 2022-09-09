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

; A function that checks whether the given value is zero (returns 1 if the given value is 0 and 0 otherwise)
(define (is-zero val)
  (cond
    [(= val 0) 1]
    [else 0]))

; A function that checks whether the given value is one (returns true if the given value is 1 and false otherwise)
(define (is-one val)
  (cond
    [(= val 1) #t]
    [else #f]))

(is-one 1)

; A function that checks the parity of a number given as a list of binary digits (returns true if the given number is odd and false otherwise)
(define (binary-odd bits)
  (define (helper bits)
    (is-one (first bits)))
  (helper (reverse bits)))

(binary-odd '(1 0 1 1 0))
(binary-odd '(1 0 1 1 1))

; A function that returns the number of trailing zeroes in a binary number given as a list of binary digits
(define (count-trailing-zeros bits)
  (define (helper bits current)
  (cond
    [(= (first bits) 1) current]
    [else (helper (rest bits)
                  (+ current (is-zero (first bits))))]))
    (helper (reverse bits) 0))

(count-trailing-zeros '(1 0 0 1 1 0 1 0 0 0))

; A function that strips a binary number given as a list of binary digits of its trailing zeros
(define (remove-trailing-zeros bits)
  (define (helper bits n)
    (cond
      [(= n 0) (reverse bits)]
      [else (helper (rest bits) (- n 1))]))
  (helper (reverse bits) (count-trailing-zeros bits)))
(remove-trailing-zeros '(0 1 0 0 1 0 0))

; A function that returns the number of leading zeroes in a binary number given as a list of binary digits
(define (count-leading-zeros bits)
  (define (helper bits current)
  (cond
    [(= (first bits) 1) current]
    [else (helper (rest bits)
                  (+ current (is-zero (first bits))))]))
    (helper bits 0))

(count-leading-zeros '(0 0 1 1 0 1 0 0 0))

; A function that strips a binary number given as a list of binary digits of its leading zeros
(define (remove-leading-zeros bits)
  (define (helper bits n)
    (cond
      [(= n 0) bits]
      [else (helper (rest bits) (- n 1))]))
  (helper bits (count-leading-zeros bits)))

(remove-leading-zeros '(0 0 1 0 1 0 1 1 0))


; A function that increments a binary number given as a list of binary digits by 1
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

; A function that decrements a binary number given as a list of binary digits by 1
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

; A more concise and efficient version of the original decrement function
(define (easy-decrement bits)
  (cond
    [(= (binary-to-decimal bits) 0) (list 0)]
    [(= (binary-to-decimal bits) 1) (list 0)]
    [else (decimal-to-binary (- (binary-to-decimal bits) 1))])
  )

(easy-decrement '(1 0 1 1 0))

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