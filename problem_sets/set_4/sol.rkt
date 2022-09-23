#lang slideshow
;1

;1.a
;I implemented this function previously for Assignment 1, it appends a list/element to the end of a given list
(define (my-append expr expr-2)
  (cond
    [(list? expr-2)
     (append expr expr-2)]
    [else
     (append expr (list expr-2))]))

(define (replicate num val)
  (define (replicate-helper num-2 res)
    (cond
      [(zero? num-2)
       res]
      [else
       (replicate-helper (- num-2 1) (my-append res val))]))
  (replicate-helper num empty))

;(replicate 10 'a)
;(replicate 3 '(1 . 2))

;1.b
(define (split lis num)
  (define (split-helper num-2 res lis-2)
    (cond
      [(or
        (zero? num-2)
        (empty? lis-2))
       (cons res (list lis-2))]
      [else
       (split-helper (- num-2 1) (my-append res (first lis-2)) (rest lis-2))]))
  (split-helper num empty lis))

;(split '(1 2 3 4 5) 2)
;(split '(a b c d) 4)
;(split '(a b c) 4)
;(split '(a b c) 0)

;1.c
;Appends a list-2 to list-1 only if list-2 is non-empty
(define (append-non-empty list-1 list-2)
  (cond
    [(empty? list-2)
     list-1]
    [else
     (append list-1 list-2)]))

(define (chunks given-list chunk-size)
  (define (chunks-helper current-list current-chunk current-result)
    (cond
      [(empty? current-list)
       (append-non-empty current-result (list current-chunk))]
      [(= (length current-chunk) chunk-size)
       (chunks-helper (rest current-list) (list (first current-list)) (append current-result (list current-chunk)))]
      [else
       (chunks-helper (rest current-list) (my-append current-chunk (first current-list)) current-result)]))
  (chunks-helper given-list empty empty))

;(chunks '(1 2 3 4 5) 2)
;(chunks '(a b c d e f) 3)

;1.d
;Generates a single "window" of a given size from the beginning of a given list, to be called recursively by windows function
(define (generate-window given-list window-size)
  (define (generate-window-helper current-list current-result)
    (cond
      [(or
        (empty? current-list)
        (= (length current-result) window-size))
       current-result]
      [else
       (generate-window-helper (rest current-list) (my-append current-result (first current-list)))]))
  (generate-window-helper given-list empty))

;(generate-window '(1 2 3 4 5) 2)

(define (windows given-list window-size)
  (define (windows-helper current-list current-result)
    (cond
      [(or
        (empty? current-list)
        (< (length current-list) window-size))
       current-result]
      [else
       (windows-helper (rest current-list) (append current-result (list (generate-window current-list window-size))))]))
  (windows-helper given-list empty))

;(windows '(1 2 3 4 5) 2)
;(windows '(a b c d e) 3)

;2

;2.a
(define (pairs given-list)
  (second (foldl (lambda (element result)
           (cons (append (first result) (list element)) (list (append (second result) (map (lambda (map-element)
                                                                                             (cons map-element element))
                                                                                           (first result))))))
         (list null null)
         given-list)))

;(pairs '(a b c d))

;2.b
;(define (splits given-list)
;  (second (foldl (lambda (element result)
;                   (cons (append (first result) (list element)) (list (append (second result) (list (split given-list (length (first result))))))))
;          (list null null)
;          given-list)))
;(splits '(a b c))

(define (splits given-list)
  (map (lambda (element) (split given-list element)) (build-list (+ (length given-list) 1) values)))
;(splits '(a b c))

;2.c
(define (perform-op given-pair op)
  (op (car given-pair) (cdr given-pair)))

(define (max-product given-list)
  (define pair-list (pairs given-list))
  (foldl (lambda (current-pair result)
           (cond
             [(> (perform-op current-pair *) (perform-op result *))
              current-pair]
             [else
              result]))
         (first pair-list)
         pair-list))

;(max-product '(1 2 3 4 3 2 1))

;2.d
(define (max-binary-op op given-list)
  (define pair-list (pairs given-list))
  (foldl (lambda (current-pair result)
           (cond
             [(> (perform-op current-pair op) (perform-op result op))
              current-pair]
             [else
              result]))
         (first pair-list)
         pair-list))

;(max-binary-op * '(1 2 3 4 3 2 1))
;(max-binary-op - '(1 2 3 4 3 2 1))

;3

;3.a
(define (max given-list)
  (foldl (lambda (element result)
           (cond
             [(> element result)
              element]
             [else
              result]))
         (first given-list)
         given-list))
;(max '(1 5 3 6 2 0))

;3.b
(define (second-max given-list)
  (cdr (foldl (lambda (element result)
           (cond
             [(and
               (> element (cdr result))
               (< element (car result)))
              (cons (car result) element)]
             [else
              result]))
         (cons (max given-list) (first given-list))
         given-list)))
;(second-max '(1 5 3 6 2 0))

;3.c
(define (top-3 given-list)
  (cons (max given-list)
        (foldl (lambda (element result)
           (cond
             [(and
               (> element (cadr result))
               (< element (car result)))
              (cons (car result) element)]
             [else
              result]))
         (cons (second-max given-list) (list (first given-list)))
         given-list)))
;(top-3 '(5 3 6 2 8 1 0))

;3.d

;3.e
(define (cumulative-sums given-list)
  (second (foldl (lambda (element result)
                   (cons (+ element (first result)) (list (my-append (second result) (+ element (first result))))))
                 (cons 0 (list '(0)))
                 given-list)))

;(cumulative-sums '(1 2 3 4 5))