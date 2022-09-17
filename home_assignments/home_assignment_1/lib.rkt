#lang slideshow
;; a.rkt
(provide f)
(define (f x)
  (displayln (add1 x)))

;; b.rkt
(require "a.rkt")
(f 3) ; => 4