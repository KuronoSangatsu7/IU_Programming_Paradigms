#lang slideshow
(define (render-bit val)
  (cond
    [(= val 0)
  (colorize (filled-rectangle 8 8) "white" )]
    [(= val 1)(colorize (filled-rectangle 8 8) "black" )]))