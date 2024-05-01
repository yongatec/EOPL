#lang racket
(require rackunit)

(define (invert lst)
 (cond [(empty? lst) empty]
       [else (cons (list (car (cdr (first lst))) (car (first lst))) (invert (rest lst)))]))


(check-equal? (invert '((a 1) (b 2) (c 3))) '((1 a) (2 b) (3 c)))