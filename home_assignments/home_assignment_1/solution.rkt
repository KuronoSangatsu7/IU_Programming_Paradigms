#lang slideshow

;1.1
;Takes a datum of length one and decides if it's a variable or not
(define (variable? expr)
  (cond
    [(or (list? expr)
         (number? expr))
     #f]
    [else #t]))
(variable? 1)
(variable? 'x)

;Takes an expression and decides if it's a sum or not
(define (sum? expr)
  (cond
    [(and (list? expr)
          (equal? (car expr) '+))
     #t]
    [else #f]))
(sum? '(+ 1 2))
(sum? '(* 1 2))

;Takes an expression and decides if it's a multiplication or not
(define (product? expr)
  (cond
    [(and (list? expr)
          (equal? (car expr) '*))
     #t]
    [else #f]))
(product? '(+ 1 2))
(product? '(* 1 2))

;Takes a sum and returns the first summand
(define (summand-1 expr)
  (cadr expr))
(summand-1 '(+ 1 2))
(summand-1 '(+ 5 6 7 8))

;Takes a sum and returns the second summand
(define (summand-2 expr)
  (caddr expr))
(summand-2 '(+ 1 2))
(summand-2 '(+ 5 6 7 8))

;Takes a multiplication and returns the first multiplier
(define (multiplier-1 expr)
  (cadr expr))
(multiplier-1 '(* 1 2))
(multiplier-1 '(* 5 6 7 8))

;Takes a multiplication and returns the second multiplier
(define (multiplier-2 expr)
  (caddr expr))
(multiplier-2 '(* 1 2))
(multiplier-2 '(* 5 6 7 8))

;1.2
;Derives a variable with respect to a given variable
(define (derive-var var respect-to)
  (cond
    [(equal? var respect-to)
     1]
    [else
     0]))
(derive-var 'x 'x)

;Derives an expression with respect to a given variable
;TODO: Consider deleting the first condition (empty?)
(define (derivative expr var)
  (cond
    [(empty? expr)
     empty]
    [(list? expr)
     (cond
       [(sum? expr)
        (list '+ (derivative (summand-1 expr) var) (derivative (summand-2 expr) var))]
       [(product? expr)
        (list '+ (list '* (derivative (multiplier-1 expr) var) (multiplier-2 expr)) (list '* (multiplier-1 expr) (derivative (multiplier-2 expr) var)))])]
    [else
     (cond
       [(variable? expr)
        (derive-var expr var)]
       [else
        0])]))
(derivative '(+ 1 x) 'x)
(derivative '(* 2 y) 'y)
(derivative '(* (+ x y) (+ x (+ x x))) 'x)

;1.3
;Computes the sum of a given expression at the top level according to the specified rules.
(define (compute-sum expr)
  (cond
    [(and
      (number? (cadr expr))
      (number? (caddr expr)))
     (+ (cadr expr) (caddr expr))]
    [else
     (cond
       [(or
         (equal? 0 (cadr expr))
         (equal? 0 (caddr expr)))
        (cond
          [(equal? 0 (cadr expr))
           (caddr expr)]
          [else
           (cadr expr)])]
       [else
        expr])]))
(compute-sum '(+ 1 2))
(compute-sum '(+ x 0))
(compute-sum '(+ 0 x))
(compute-sum '(+ (+ x (+ y 1)) 0))

;Computes the product of a given expression at the top level according to the specified rules.
(define (compute-prod expr)
  (cond
    [(and
      (number? (cadr expr))
      (number? (caddr expr)))
     (* (cadr expr) (caddr expr))]
    [else
     (cond
       [(or
         (equal? 0 (cadr expr))
         (equal? 0 (caddr expr)))
        0]
       [else
        (cond
          [(or
            (equal? 1 (cadr expr))
            (equal? 1 (caddr expr)))
           (cond
             [(equal? 1 (cadr expr))
              (caddr expr)]
             [else
              (cadr expr)])]
          [else
           expr])])]))
(compute-prod '(* x 1))
(compute-prod '(* 1 x))
(compute-prod '(* 3 3))
(compute-prod '(* 0 x))
(compute-prod '(* (+ x y) 0))
(compute-prod '(* 1 (+ x y)))

;Computes the sum or product of a given expression at the top level according to the specified rules. To be called recursively by the simplify function
(define (compute-at-root expr)
  (cond
    [(sum? expr)
     (compute-sum expr)]
    [(product? expr)
     (compute-prod expr)]))

(compute-at-root '(+ 1 2))
(compute-at-root '(+ x 0))
(compute-at-root '(+ 0 x))
(compute-at-root '(+ (+ x (+ y 1)) 0))
(compute-at-root '(* x 1))
(compute-at-root '(* 1 x))
(compute-at-root '(* 3 3))
(compute-at-root '(* 0 x))
(compute-at-root '(* (+ x y) 0))
(compute-at-root '(* 1 (+ x y)))

;Simplifies a given expression according to the given rules
(define (simplify expr)
  (cond
    [(not (list? expr))
     expr]
    [else
     (compute-at-root (list (car expr) (simplify (cadr expr)) (simplify (caddr expr))))]))

(simplify '(+ 0 1))
(simplify '(+ (* 0 y) (* 2 1)))
(simplify '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1)))))

