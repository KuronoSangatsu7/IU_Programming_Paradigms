#lang slideshow
(define (decimal-to-binary n)
  (define (helper current-list n)
    (cond
      [(< n 2) (reverse (append current-list (list n)))]
      [else (helper (append current-list (list (remainder n 2))) (quotient n 2))])
    )
  (helper empty n))
