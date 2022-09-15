#lang slideshow

(define (len-via-map some-list)
  (apply + (map (lambda (elem) 1) some-list)))

(len-via-map '(0 1 0 1))