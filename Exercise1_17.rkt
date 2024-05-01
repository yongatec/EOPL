#lang racket 
(require rackunit)

(define (down lst)
 (cond [(empty? lst) empty]
       [else (cons (list (first lst)) (down (rest lst)))]))


(check-equal? (down '(1 2 3)) '((1) (2) (3)))