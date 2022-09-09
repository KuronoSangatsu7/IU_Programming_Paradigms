#lang slideshow
(define (render-bit val)
  (cond
    [(= val 0)
  (colorize (filled-rectangle 8 8) "white" )]
    [(= val 1)(colorize (filled-rectangle 8 8) "black" )]))

(define (render-bits bits)
  (cond
    [(empty? bits) (hc-append)]
    [else (hc-append 10 (render-bit (first bits))
                     (render-bits(rest bits)))]))
(render-bits '(1 0 0 1 0 1 0 1))