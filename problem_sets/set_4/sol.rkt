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
(define (splits given-list)
  (append (second (foldl (lambda (element result)
                   (cons (append (first result) (list element)) (list (append (second result) (list (split given-list (length (first result))))))))
          (list null null)
          given-list)) (list (split given-list (length given-list)))))
;(splits '(a b c))

; A cleaner implementation using build-list
;(define (splits given-list)
;  (map (lambda (element) (split given-list element)) (build-list (+ (length given-list) 1) values)))
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

;2.e
;The idea is to generate a list of binary numbers up to (expt 2 (length given-list)), filter out the binary numbers with (= (count 1 number) given-size)
;and then generate combinations per the filtered binary numbers

;Converts a given decimal number to its binary bit-array representation
(define (dec-to-bin num)
  (cond
    [(equal? num 0)
     (list 0)]
    [else
     (define divisions (replicate (+ (floor (log num 2)) 1) 1))
     (reverse (car (foldl (lambda (element result)
                            (cons (append (first result) (list (remainder (second result) 2))) (list (quotient (second result) 2))))
                          (cons null (list num))
                          divisions)))]))

;(dec-to-binary 55)

;Returns a list of the numbers 0 through num
(define (count-to num)
  (define initial-list (replicate num 0))
  (second (foldl (lambda (element result)
                   (cons (+ 1 (first result)) (list (my-append (second result) (+ 1 (first result))))))
                 (cons 0 (list '(0)))
                 initial-list)))
;(count-to 5)

;Counts the number of 1 bits in a binary number
(define (count-1s given-binary)
  (foldl (lambda (element result)
                   (+ result element))
                 0
                 given-binary))
;(count-1s '(1 0 1 1 0))

;Returns a list of binary numbers with the specified number of 1s up to a given number
;Example: (binary-num-1s 10 2) -> '((1 1) (1 0 1) (1 1 0) (1 0 0 1) (1 0 1 0))
(define (binary-num-1s num num-bits)
  (define numbers (count-to num))
  (define binary-numbers (map (lambda (element)
                                (dec-to-bin element))
                              numbers))
  (filter (lambda (element)
            (equal? (count-1s element) num-bits))
          binary-numbers))

;(binary-num-1s 10 2)

;Returns a combination based on a binary representation of a number
;Example: (generate-combination '(a b c) '(1 1 0)) -> '(a b)
(define (generate-combination given-list binary-num-1)
  (define binary-num (cond
                       [(< (length binary-num-1) (length given-list))
                          (append (replicate (- (length given-list) (length binary-num-1)) 0) binary-num-1)]
                       [else
                        binary-num-1]))
  (filter (lambda (element)
            (not (empty? element)))
          (map (lambda (element binary-num)
         (cond
           [(= binary-num 1)
            element]
           [else
            null]))
       given-list
       binary-num)))

;(generate-combination '(a b c) '(1 1 0))

(define (combinations given-list given-length)
  (define combinations-binary (binary-num-1s (expt 2 (length given-list)) given-length))
  (map (lambda (element)
         (generate-combination given-list element))
       combinations-binary))

(combinations '(a b c d) 3)

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
(define (group given-list)
  (define groups-result (foldl (lambda (element result)
                   (cond
                     [(equal? element (car (car result)))
                      (cons (append (car result) (list element)) (cdr result))]
                     [else
                      (cons (list element) (list (append (second result) (list (car result)))))]))
                 (cons (list (first given-list)) (list null))
                 (rest given-list)))
  (append (second groups-result) (list (first groups-result))))

;(group '(a b b c c c b a a))

;3.e
(define (cumulative-sums given-list)
  (second (foldl (lambda (element result)
                   (cons (+ element (first result)) (list (my-append (second result) (+ element (first result))))))
                 (cons 0 (list '(0)))
                 given-list)))

;(cumulative-sums '(1 2 3 4 5))