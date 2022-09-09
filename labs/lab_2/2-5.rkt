#lang slideshow

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