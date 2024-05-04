#lang racket
(require rackunit)

;(define (every? pred lst) #f)

(define (every? pred lst)
    (cond [(empty? lst) #t]
           [else (and (pred (first lst)) (every? pred (rest lst)))]))

(check-equal? (every? number? '(a b c 3 e)) #f)
(check-equal? (every? number? '(1 2 3 5 3)) #t)

