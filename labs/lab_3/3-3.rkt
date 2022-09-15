#lang slideshow

(define (average some-list)
  (/ (apply + some-list) (len-via-foldl some-list)))

(average `(1 2 3 4))