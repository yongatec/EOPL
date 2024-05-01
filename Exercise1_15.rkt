#lang racket
(require rackunit)

(define (duple n x)
 (cond [(zero? n) empty]
       [else (cons x (duple (- n 1) x))]))

(check-equal? (duple 3 '(ha ha)) '((ha ha) (ha ha) (ha ha)))
(check-equal? (duple 5 1) '(1 1 1 1 1))