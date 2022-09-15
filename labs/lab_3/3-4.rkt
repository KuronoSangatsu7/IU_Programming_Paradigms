#lang slideshow

(define (helper-filter also-a-list val)
    (cons val
    (filter (lambda (elem, val)
              (cond
                [(equal? elem val) #f]
                [else #t])) (rest also-a-list))))
(helper-filter '(1 2 3 4 5 1 1) (first '(1 2 3 4 5 1 1)))

(define (my-remove-duplicates some-list)
  (define (helper also-a-list res)
    (cond
      [(empty? also-a-list) (reverse res)]
      [else (helper (rest (helper-filter also-a-list (first also-a-list))) (cons (first also-a-list) res))]))
  (helper some-list empty))

(my-remove-duplicates '(1 2 4 5 1 4 5 3 3 1 1 1 2))