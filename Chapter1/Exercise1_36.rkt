#lang racket
(require rackunit)

(define (g hd tl)
  (cons hd (map (lambda (item) (list (+ (car item) 1) (cadr item))) tl)))


(define number-elements
  (lambda (lst)
    (if (empty? lst)
        empty
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

;;2.Examples
(check-equal? (number-elements '(a b c d e)) '((0 a) (1 b) (2 c) (3 d) (4 e)))