#lang slideshow
; Checking whether a given value is zero
(define (is-zero val)
  (cond
    [(= val 0) 1]
    [else 0]))

; Counting the number of leading zeros
(define (count-leading-zeros bits)
  (define (helper bits current)
  (cond
    [(= (first bits) 1) current]
    [else (helper (rest bits)
                  (+ current (is-zero (first bits))))]))
    (helper bits 0))

; Removing leading zeros
(define (remove-leading-zeros bits)
  (define (helper bits n)
    (cond
      [(= n 0) bits]
      [else (helper (rest bits) (- n 1))]))
  (helper bits (count-leading-zeros bits)))

(define (encode-with-lengths bits)
  (define (helper bits cnt ans)
    (cond
      [(empty? (rest bits)) (append ans (list (+ cnt 1)))]
      [else
       (cond
         [(= (first bits) (second bits)) (helper (rest bits) (+ cnt 1) ans)]
         [else (helper (rest bits) 0 (append ans (list (+ cnt 1))))]
         )]))
  (helper bits 0 empty))

(encode-with-lengths (remove-leading-zeros '(0 0 0 1 1 0 1 1 1 0 0)))