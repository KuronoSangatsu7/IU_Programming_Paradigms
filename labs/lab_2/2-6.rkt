#lang slideshow

(define (decimal-to-binary n)
  (define (helper current-list n)
    (cond
      [(< n 2) (reverse (append current-list (list n)))]
      [else (helper (append current-list (list (remainder n 2))) (quotient n 2))])
    )
  (helper empty n))

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

(define (create-numbers n)
  (define (helper current-list current-num n)
    (cond
      [(= current-num n) current-list]
      [else (helper (append current-list (list (increment (decimal-to-binary current-num)))) (+ 1 current-num) n)]))
  (helper empty 0 n))

(create-numbers 5)