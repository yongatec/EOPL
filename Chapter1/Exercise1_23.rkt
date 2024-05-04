#lang racket
(require rackunit)

;;4. Code the body
(define (list-index pred lst)
  (define (ind-helper lst n)
    (cond [(empty? lst) #f]
          [else (if (pred (first lst))
                    n
                    (ind-helper (rest lst) (+ n 1)))]))
 (ind-helper lst 0))

;;2.Examples
(check-equal? (list-index number? '(a 2 (1 3) b 7)) 1)
(check-equal? (list-index symbol? '(a (b c) 17 foo)) 0)
(check-equal? (list-index symbol? '(1 2 (a b) 3)) #f)

#|
;;3 Template
(define (list-index pred lst)
 (cond [(empty? lst) (...)]
       [else (... pred 
                  (first lst)
                   (list-index pred (rest lst)))]))
|#
