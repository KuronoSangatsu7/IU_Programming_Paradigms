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
