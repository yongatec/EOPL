#lang racket
(require rackunit)

(define (remove-first los s)
 (cond [(empty? los) empty]
       [(eq? s (first los)) (cdr los)]
       [else (cons (first los) (remove-first (rest los) s))]))

(check-equal? (remove-first '( 1 2 3 4 3) 3) '(1 2 4 3))

(define (remove-all los s)
 (cond [(empty? los) empty]
       [(eq? (first los) s) (remove-all (rest los) s)]
       [else (cons (first los) (remove-all (rest los) s))]))

(check-equal? (remove-all '( 1 2 3 4 3) 3) '(1 2 4))