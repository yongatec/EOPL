#lang racket
(require rackunit)

;(define (exists? pred lst) #t)

(define (exists? pred lst)
 (cond [(empty? lst) #f]
       [else (or (pred (first lst)) (exists? pred (rest lst)))]))


(check-equal? (exists? number? '(a b c 3 e)) #t)
(check-equal? (exists? number? '(a b c d e)) #f)

