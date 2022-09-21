; Author: Jaffar Totanji - SD-01
; j.totanji@innopolis.university

#lang slideshow
;1.1
;Takes a datum of length one and decides if it's a variable or not
(define (variable? expr)
  (cond
    [(or (list? expr)
         (number? expr)
         (equal? expr '+)
         (equal? expr '*)
         (equal? expr 'log)
         (equal? expr '^)
         (equal? expr 'sin)
         (equal? expr 'cos)
         (equal? expr 'tan))
     #f]
    [else #t]))

;Testing
(pretty-print '((variable? 1)))
(variable? 1)

(pretty-print '((variable? 'x)))
(variable? 'x)

;Takes an expression and decides if it's a sum or not
(define (sum? expr)
  (cond
    [(and (list? expr)
          (> (length expr) 2)
          (equal? (car expr) '+))
     #t]
    [else #f]))

;Testing
(pretty-print '((sum? '(+ 1 2))))
(sum? '(+ 1 2))

(pretty-print '((sum? '(* 1 2))))
(sum? '(* 1 2))

;Takes an expression and decides if it's a multiplication or not
(define (product? expr)
  (cond
    [(and (list? expr)
          (> (length expr) 2)
          (equal? (car expr) '*))
     #t]
    [else #f]))

;Testing
(pretty-print '((product? '(+ 1 2))))
(product? '(+ 1 2))

(pretty-print '(product? '(* 1 2)))
(product? '(* 1 2))

;Takes a sum and returns the first summand
(define (summand-1 expr)
  (cadr expr))

;Testing
(pretty-print '(summand-1 '(+ 1 2)))
(summand-1 '(+ 1 2))

(pretty-print '(summand-1 '(+ 5 6 7 8)))
(summand-1 '(+ 5 6 7 8))

;Takes a sum and returns the second summand
(define (summand-2 expr)
  (caddr expr))

;Testing
(pretty-print '((summand-2 '(+ 1 2))))
(summand-2 '(+ 1 2))

(pretty-print '((summand-2 '(+ 5 6 7 8))))
(summand-2 '(+ 5 6 7 8))

;Takes a multiplication and returns the first multiplier
(define (multiplier-1 expr)
  (cadr expr))

;Testing
(pretty-print '((multiplier-1 '(* 1 2))))
(multiplier-1 '(* 1 2))

(pretty-print '((multiplier-1 '(* 5 6 7 8))))
(multiplier-1 '(* 5 6 7 8))

;Takes a multiplication and returns the second multiplier
(define (multiplier-2 expr)
  (caddr expr))

;Testing
(pretty-print '((multiplier-2 '(* 1 2))))
(multiplier-2 '(* 1 2))

(pretty-print '((multiplier-2 '(* 5 6 7 8))))
(multiplier-2 '(* 5 6 7 8))

;1.2
;Derives a variable with respect to a given variable
(define (derive-var var respect-to)
  (cond
    [(equal? var respect-to)
     1]
    [else
     0]))

;Testing
(pretty-print '((derive-var 'x 'x)))
(derive-var 'x 'x)

;Derives an expression with respect to a given variable
(define (derivative expr var)
  (cond
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

;Testing
(pretty-print '((derivative '(+ 1 x) 'x)))
(derivative '(+ 1 x) 'x)

(pretty-print '((derivative '(* 2 y) 'y)))
(derivative '(* 2 y) 'y)

(pretty-print '((derivative '(* (+ x y) (+ x (+ x x))) 'x)))
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

;Testing
(pretty-print '((compute-sum '(+ 1 2))))
(compute-sum '(+ 1 2))

(pretty-print '((compute-sum '(+ x 0))))
(compute-sum '(+ x 0))

(pretty-print '((compute-sum '(+ 0 x))))
(compute-sum '(+ 0 x))

(pretty-print '((compute-sum '(+ (+ x (+ y 1)) 0))))
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

;Testing
(pretty-print '((compute-prod '(* x 1))))
(compute-prod '(* x 1))

(pretty-print '((compute-prod '(* 1 x))))
(compute-prod '(* 1 x))

(pretty-print '((compute-prod '(* 3 3))))
(compute-prod '(* 3 3))

(pretty-print '((compute-prod '(* 0 x))))
(compute-prod '(* 0 x))

(pretty-print '((compute-prod '(* (+ x y) 0))))
(compute-prod '(* (+ x y) 0))

(pretty-print '((compute-prod '(* 1 (+ x y)))))
(compute-prod '(* 1 (+ x y)))

;Computes the sum or product of a given expression at the top level according to the specified rules. To be called recursively by the simplify function
(define (compute-at-root expr)
  (cond
    [(sum? expr)
     (compute-sum expr)]
    [(product? expr)
     (compute-prod expr)]))

;Testing
(pretty-print '((compute-at-root '(+ 1 2))))
(compute-at-root '(+ 1 2))

(pretty-print '((compute-at-root '(+ x 0))))
(compute-at-root '(+ x 0))

(pretty-print '((compute-at-root '(+ 0 x))))
(compute-at-root '(+ 0 x))

(pretty-print '((compute-at-root '(+ (+ x (+ y 1)) 0))))
(compute-at-root '(+ (+ x (+ y 1)) 0))

(pretty-print '((compute-at-root '(* x 1))))
(compute-at-root '(* x 1))

(pretty-print '((compute-at-root '(* 1 x))))
(compute-at-root '(* 1 x))

(pretty-print '((compute-at-root '(* 3 3))))
(compute-at-root '(* 3 3))

(pretty-print '((compute-at-root '(* 0 x))))
(compute-at-root '(* 0 x))

(pretty-print '((compute-at-root '(* (+ x y) 0))))
(compute-at-root '(* (+ x y) 0))

(pretty-print '((compute-at-root '(* 1 (+ x y)))))
(compute-at-root '(* 1 (+ x y)))


;Simplifies a given expression according to the given rules
(define (simplify expr)
  (cond
    [(not (list? expr))
     expr]
    [else
     (compute-at-root (list (car expr) (simplify (cadr expr)) (simplify (caddr expr))))]))

;Testing
(pretty-print '((simplify '(+ 0 1))))
(simplify '(+ 0 1))

(pretty-print '((simplify '(+ (* 0 y) (* 2 1)))))
(simplify '(+ (* 0 y) (* 2 1)))

(pretty-print '((simplify '(+ (* x y) (* 2 1)))))
(simplify '(+ (* x y) (* 2 1)))

(pretty-print '((simplify '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1)))))))
(simplify '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1)))))

;1.5
;Swaps the first and second elements of a list
(define (swap expr)
  (append (list (cadr expr) (car expr)) (cddr expr)))

;Testing
(pretty-print '((swap '(1 2 3 4 5))))
(swap '(1 2 3 4 5))

;Converts a given expression to infix form
(define (to-infix expr)
  (cond
    [(not (list? expr))
     expr]
    [else
     (swap (list (car expr) (to-infix (cadr expr)) (to-infix (caddr expr))))]))

;Testing
(pretty-print '((to-infix '(+ (+ x (+ x x)) (* (+ x y) 3)))))
(to-infix '(+ (+ x (+ x x)) (* (+ x y) 3)))

;1.6
;Takes an expression and decides if it's an exponentiation or not
(define (exponent? expr)
  (cond
    [(and (list? expr)
          (= (length expr) 3)
          (equal? (car expr) '^))
     #t]
    [else #f]))

;Testing
(pretty-print '((exponent? '(+ 1 2))))
(exponent? '(+ 1 2))

(pretty-print '((exponent? '(^ 1 2))))
(exponent? '(^ 1 2))

;Takes an expression and decides if it's a sine or not
(define (sin? expr)
  (cond
    [(and (list? expr)
          (= (length expr) 2)
          (equal? (car expr) 'sin))
     #t]
    [else #f]))

;Testing
(pretty-print '((sin? '(+ 1 2))))
(sin? '(+ 1 2))

(pretty-print '((sin? '(sin 1))))
(sin? '(sin 1))

;Takes an expression and decides if it's a cosine or not
(define (cos? expr)
  (cond
    [(and (list? expr)
          (= (length expr) 2)
          (equal? (car expr) 'cos))
     #t]
    [else #f]))

;Testing
(pretty-print '((cos? '(^ 1 2))))
(cos? '(^ 1 2))

(pretty-print '((cos? '(cos 1))))
(cos? '(cos 1))

;Takes an expression and decides if it's a tangent or not
(define (tan? expr)
  (cond
    [(and (list? expr)
          (= (length expr) 2)
          (equal? (car expr) 'tan))
     #t]
    [else #f]))

;Testing
(pretty-print '((tan? '(* 1 2))))
(tan? '(* 1 2))

(pretty-print '((tan? '(tan 1))))
(tan? '(tan 1))

;Takes an expression and decides if it's a natural logarithm or not
(define (log? expr)
  (cond
    [(and (list? expr)
          (= (length expr) 2)
          (equal? (car expr) 'log))
     #t]
    [else #f]))

;Testing
(pretty-print '((log? '(* 1 2))))
(log? '(* 1 2))

(pretty-print '((log? '(log 1))))
(log? '(log 1))

;Derives an exponentiation with respect to a given variable
(define (derive-exponent expr var)
  (list '* expr (list '+ (list '* (caddr expr) (list '* (derivative-extended (cadr expr) var) (list '^ (cadr expr) -1))) (list '* (derivative-extended (caddr expr) var) (list 'log (cadr expr))))))

;Derives a sine with respect to a given variable
(define (derive-sine expr var)
  (list '* (list 'cos (cadr expr)) (derivative-extended (cadr expr) var)))

;Derives a cosine with respect to a given variable
(define (derive-cosine expr var)
  (list '* (derivative-extended (cadr expr) var) (list '* -1 (list 'sin (cadr expr)))))

;Derives a tangent with respect to a given variable
(define (derive-tangent expr var)
  (list '* (derivative-extended (cadr expr) var) (list '^ (list '^ (list 'cos (cadr expr)) 2) -1)))

;Derives a natural logarithm with respect to a given variable
(define (derive-log expr var)
  (list '* (derivative-extended (cadr expr) var) (list '^ (cadr expr) -1)))

;Derives an expression with respect to a given variable
(define (derivative-extended expr var)
  (cond
    [(list? expr)
     (cond
       [(sum? expr)
        (list '+ (derivative-extended (summand-1 expr) var) (derivative-extended (summand-2 expr) var))]
       [(product? expr)
        (list '+ (list '* (derivative-extended (multiplier-1 expr) var) (multiplier-2 expr)) (list '* (multiplier-1 expr) (derivative-extended (multiplier-2 expr) var)))]
       [(exponent? expr)
        (derive-exponent expr var)]
       [(sin? expr)
        (derive-sine expr var)]
       [(cos? expr)
        (derive-cosine expr var)]
       [(tan? expr)
        (derive-tangent expr var)]
       [(log? expr)
        (derive-log expr var)])]
    [else
     (cond
       [(variable? expr)
        (derive-var expr var)]
       [else
        0])]))

;Testing
(pretty-print '((derivative-extended '(+ 1 x) 'x)))
(derivative-extended '(+ 1 x) 'x)

(pretty-print '((derivative-extended '(* 2 y) 'y)))
(derivative-extended '(* 2 y) 'y)

(pretty-print '((derivative-extended '(* (+ x y) (+ x (+ x x))) 'x)))
(derivative-extended '(* (+ x y) (+ x (+ x x))) 'x)

(pretty-print '((derivative-extended '(+ x (cos x)) 'x)))
(derivative-extended '(+ x (cos x)) 'x)

(pretty-print '((derivative-extended '(+ x (cos (* x y))) 'x)))
(derivative-extended '(+ x (cos (* x y))) 'x)

(pretty-print '((derivative-extended '(+ x (sin x)) 'x)))
(derivative-extended '(+ x (sin x)) 'x)

(pretty-print '((derivative-extended '(+ x (tan x)) 'x)))
(derivative-extended '(+ x (tan x)) 'x)

(pretty-print '((derivative-extended '(+ x (^ x y)) 'x)))
(derivative-extended '(+ x (^ x y)) 'x)

(pretty-print '((derivative-extended '(+ x (log x)) 'x)))
(derivative-extended '(+ x (log x)) 'x)

;Computes the exponentiation of a given expression at the top level
(define (compute-exp expr)
  (cond
    [(and
      (not (list? (caddr expr)))
      (equal? (caddr expr) 0))
     1]
    [(and
      (not (list? (caddr expr)))
      (equal? (caddr expr) 1))
     (cadr expr)]
    [(and
      (number? (cadr expr))
      (number? (caddr expr)))
     (expt (cadr expr) (caddr expr))]
    [else
     expr]))

(pretty-print '(compute-exp '(^ (* y z) 0)))
(compute-exp '(^ (* y z) 0))

(pretty-print '(compute-exp '(^ (* y z) 1)))
(compute-exp '(^ (* y z) 1))

(pretty-print '((compute-exp '(^ 2 3))))
(compute-exp '(^ 2 3))

;Functions to compute the numeric value of trigonometric functions and natural logarithm when applied to constants at the top level
(define (compute-sin expr)
  (cond
    [(and
      (not (list? (cadr expr)))
      (number? (cadr expr)))
     (sin (cadr expr))]
    [else
     expr]))

(define (compute-cos expr)
  (cond
    [(and
      (not (list? (cadr expr)))
      (number? (cadr expr)))
     (cos (cadr expr))]
    [else
     expr]))

(define (compute-tan expr)
  (cond
    [(and
      (not (list? (cadr expr)))
      (number? (cadr expr)))
     (tan (cadr expr))]
    [else
     expr]))

(define (compute-log expr)
  (cond
    [(and
      (not (list? (cadr expr)))
      (number? (cadr expr)))
     (log (cadr expr))]
    [else
     expr]))

;Testing
(pretty-print '((compute-sin '(sin 60))))
(compute-sin '(sin 60))

(pretty-print '((compute-cos '(cos 60))))
(compute-cos '(cos 60))

(pretty-print '((compute-tan '(tan 60))))
(compute-tan '(tan 60))

(pretty-print '((compute-log '(log 1))))
(compute-log '(log 1))

;Computes the sum/product/exponentiation/trignometric function/natural logarithm of a given expression at the top level. To be called recursively by the simplify function
(define (compute-at-root-extended expr)
  (cond
    [(sum? expr)
     (compute-sum expr)]
    [(product? expr)
     (compute-prod expr)]
    [(exponent? expr)
     (compute-exp expr)]
    [(sin? expr)
     (compute-sin expr)]
    [(cos? expr)
     (compute-cos expr)]
    [(tan? expr)
     (compute-tan expr)]
    [(log? expr)
     (compute-log expr)]))

;Simplifies a given expression according to the given rules
(define (simplify-extended expr)
  (cond
    [(not (list? expr))
     expr]
    [else
     (compute-at-root-extended (append (list (car expr)) (map (lambda (expr-1) (simplify-extended expr-1)) (rest expr))))]))

;Testing
(pretty-print '((simplify-extended '(+ x y))))
(simplify-extended '(+ x y))

(pretty-print '((simplify-extended '(+ (* (+ (^ (+ x y) 1) 0) 1) (+ (log 1) (cos 55))))))
(simplify-extended '(+ (* (+ (^ (+ x y) 1) 0) 1) (+ (log 1) (cos 55))))

(pretty-print '((simplify-extended (derivative-extended '(^ (+ x y) (* z x)) 'x))))
(simplify-extended (derivative-extended '(^ (+ x y) (* z x)) 'x))

;1.7
;Appends a given expression (element or list) to the end of a given list
(define (my-append expr expr-2)
  (cond
    [(list? expr-2)
     (append expr expr-2)]
    [else
     (foldr cons (list expr-2) expr)]))

;Testing
(pretty-print '((my-append '(1 2 3) 1)))
(my-append '(1 2 3) 1)

(pretty-print '((my-append '(1 2 3) '(1 2))))
(my-append '(1 2 3) '(1 2))

;Derives an exponentiation with respect to a given variable
(define (derive-exponent-final expr var)
  (list '* expr (list '+ (list '* (caddr expr) (derivative-final (cadr expr) var) (list '^ (cadr expr) -1)) (list '* (derivative-final (caddr expr) var) (list 'log (cadr expr))))))

;Derives a sine with respect to a given variable
(define (derive-sine-final expr var)
  (list '* (list 'cos (cadr expr)) (derivative-final (cadr expr) var)))

;Derives a cosine with respect to a given variable
(define (derive-cosine-final expr var)
  (list '* (derivative-final (cadr expr) var) (list '* -1 (list 'sin (cadr expr)))))

;Derives a tangent with respect to a given variable
(define (derive-tangent-final expr var)
  (list '* (derivative-final (cadr expr) var) (list '^ (list '^ (list 'cos (cadr expr)) 2) -1)))

;Derives a natural logarithm with respect to a given variable
(define (derive-log-final expr var)
  (list '* (derivative-final (cadr expr) var) (list '^ (cadr expr) -1)))

;Valid Expression Checker

;Checks whether a given list is a valid operation
(define (valid-operation? expr)
  (cond
    [(or
      (sum? expr)
      (product? expr)
      (log? expr)
      (sin? expr)
      (cos? expr)
      (tan? expr)
      (exponent? expr))
     #t]
    [else
     #f]))

;Checks whether a given expression is valid to be used by the program
(define (verify-expression-integrity expr)
  (cond
    [(not (list? expr))
     (cond
       [(or
         (variable? expr)
         (number? expr))
        #t]
       [else
        #f])]
    [else
     (cond
       [(valid-operation? expr)
        (andmap verify-expression-integrity (cdr expr))]
       [else
        #f])]))

;Testing
(pretty-print '((verify-expression-integrity '(+ 1 2))))
(verify-expression-integrity '(+ 1 2))

(pretty-print '((verify-expression-integrity '(+))))
(verify-expression-integrity '(+))

(pretty-print '((verify-expression-integrity '(* 1))))
(verify-expression-integrity '(* 1))

(pretty-print '((verify-expression-integrity '())))
(verify-expression-integrity '())

(pretty-print '((verify-expression-integrity 'x)))
(verify-expression-integrity 'x)

(pretty-print '((verify-expression-integrity '+)))
(verify-expression-integrity '+)

(pretty-print '((verify-expression-integrity '(* 5 (^ 2)))))
(verify-expression-integrity '(* 5 (^ 2)))

;Derives an expression with respect to a given variable
(define (derivative-final expr var)
  (cond
    [(not (verify-expression-integrity expr))
     (error "Invalid expression, please use the function verify-expression-integrity to verify your expression before performing operations on it")]
    [(list? expr)
     (cond
       [(sum? expr)
        (append '(+) (map (lambda (expr-1) (derivative-final expr-1 var)) (rest expr)))]
       [(product? expr)
        (append '(+) (map (lambda (expr-1) (append (remove expr-1 expr) (list (derivative-final expr-1 var)))) (rest expr)))]
       [(exponent? expr)
        (derive-exponent-final expr var)]
       [(sin? expr)
        (derive-sine-final expr var)]
       [(cos? expr)
        (derive-cosine-final expr var)]
       [(tan? expr)
        (derive-tangent-final expr var)]
       [(log? expr)
        (derive-log-final expr var)])]
    [else
     (cond
       [(variable? expr)
        (derive-var expr var)]
       [else
        0])]))

;Testing
(pretty-print '((derivative-final '(+ 1 x y) 'x)))
(derivative-final '(+ 1 x y) 'x)

(pretty-print '((derivative-final '(+ 1 x y (* x y z)) 'x)))
(derivative-final '(+ 1 x y (* x y z)) 'x)

;Checks whether a given expression is NOT equivalent to 0
(define (is-not-zero? expr)
  (not (equal? 0 expr)))

;Checks whether a given expression is equivalent to 0
(define (is-zero? expr)
  (equal? 0 expr))

;Checks whether a given expression is NOT equivalent to 1
(define (is-not-one? expr)
  (not (equal? 1 expr)))

;Checks whether a given expression is NOT a number
(define (is-nan? expr)
  (not (number? expr)))

;Removes a plus/multiplication sign applied to a single expression
(define (remove-redundant-sign expr)
  (cond
    [(= (length expr) 2)
     (cadr expr)]
    [(= (length expr) 1)
     empty]
    [else
     expr]))

;Computes sum of expressions at the top level
(define (compute-sum-final expr)
  (cond
    [(andmap number? expr)
     (apply + (rest expr))]
    [(ormap number? expr)
     (remove-redundant-sign (filter is-not-zero? (my-append (filter is-nan? expr) (apply + (filter number? expr)))))]
    [else
     (remove-redundant-sign (filter is-not-zero? expr))]))

;Computes product of expressions at the top level
(define (compute-prod-final expr)
  (cond
    [(andmap number? expr)
     (apply * (rest expr))]
    [(ormap is-zero? expr)
     0]
    [(ormap number? expr)
     (remove-redundant-sign (filter is-not-one? (my-append (filter is-nan? expr) (apply * (filter number? expr)))))]
    [else
     (remove-redundant-sign (filter is-not-one? expr))]))

;Computes a given expression at the top level. To be called recursively by the simplify-final function
(define (compute-at-root-final expr)
  (cond
    [(sum? expr)
     (cond
       [(empty? (compute-sum-final expr))
        0]
       [else
        (compute-sum-final expr)])]
    [(product? expr)
     (cond
       [(empty? (compute-prod-final expr))
        1]
       [else
        (compute-prod-final expr)])]
    [(exponent? expr)
     (compute-exp expr)]
    [(sin? expr)
     (compute-sin expr)]
    [(cos? expr)
     (compute-cos expr)]
    [(tan? expr)
     (compute-tan expr)]
    [(log? expr)
     (compute-log expr)]))

;Simplifies a given expression
(define (simplify-final expr)
  (cond
    [(not (verify-expression-integrity expr))
     (error "Invalid expression, please use the function verify-expression-integrity to verify your expression before performing operations on it")]
    [(not (list? expr))
     expr]
    [else
     (compute-at-root-final (append (list (car expr)) (map (lambda (expr-1) (simplify-final expr-1)) (rest expr))))]))

;Testing
(pretty-print '((simplify-final (derivative-final '(* z (+ 1 2)) 'x))))
(simplify-final (derivative-final '(* z (+ 1 2)) 'x))

(pretty-print '((simplify-final '(+ 0 1 0 (+ (* y z 1) (* x z 0) (* x y 0))))))
(simplify-final '(+ 0 1 0 (+ (* y z 1) (* x z 0) (* x y 0))))

(pretty-print '((simplify-final (derivative-final '(^ (+ (* x -1 (tan z)) y 3) (* z (+ x (log 1)) 5 2)) 'x))))
(simplify-final (derivative-final '(^ (+ (* x -1 (tan z)) y 3) (* z (+ x (log 1)) 5 2)) 'x))

;1.8
;Removes duplicates of the first element of a list (From my solutions for lab 3)
(define (helper-filter also-a-list val)
    (cons val
    (filter (lambda (elem, val)
              (cond
                [(equal? elem val) #f]
                [else #t])) (rest also-a-list))))

;Testing
(pretty-print '((helper-filter '(1 2 3 4 5 1 1) (first '(1 2 3 4 5 1 1)))))
(helper-filter '(1 2 3 4 5 1 1) (first '(1 2 3 4 5 1 1)))

;Removes duplicates from a list (From my solutions for lab 3)
(define (my-remove-duplicates some-list)
  (define (helper also-a-list res)
    (cond
      [(empty? also-a-list) (reverse res)]
      [else (helper (rest (helper-filter also-a-list (first also-a-list))) (cons (first also-a-list) res))]))
  (helper some-list empty))

;Testing
(pretty-print '((my-remove-duplicates '(1 2 4 5 1 4 5 3 3 1 1 1 2))))
(my-remove-duplicates '(1 2 4 5 1 4 5 3 3 1 1 1 2))

;Returns a list containing distinct variables occurring in an expression by order of occurrance
(define (variables-of expr)
  (cond
    [(not (list? expr))
     (cond
       [(variable? expr)
        expr]
       [else
        empty])]
    [else
     (sort (my-remove-duplicates (flatten (append (map (lambda (expr-1) (variables-of expr-1)) (cdr expr))))) #:key symbol->string string<?)]))

;Testing
(pretty-print '((variables-of '(+ 1 x y (* x y z)))))
(variables-of '(+ 1 x y (* x y z)))

;1.9
; Returns a gradient of a multivariable expression given explicitly a list of variables
(define (gradient expr vars)
  (map (lambda (var) (simplify-final (derivative-final expr var))) vars))

;Testing
(pretty-print '((gradient '(+ 1 x y (* x y z)) '(x y z))))
(gradient '(+ 1 x y (* x y z)) '(x y z))
