#lang slideshow

(define (len-via-foldl some-list)
  (foldl (lambda (elem v) 
            (+ 1 v)) 0 some-list))

(len-via-foldl '(0 1 0 1 2 3 5 6))
