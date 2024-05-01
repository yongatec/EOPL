#lang racket
(require rackunit)

;;(list-set lst n x) returns a list like lst, except that n-th element is x

;;1.SPS
;; Signature : ListOfX Int Y -> ListOfZ
;; Purpose: It returns a new list by changing the nth element with x
;; Stub
;(define (list-set lst n x) lst)

(define (list-set lst n x)
 (cond [(empty? lst) empty]
       [(zero? n) (cons x (rest lst))]
       [else (cons (first lst) (list-set (rest lst) (- n 1) x))]))

;;2.Examples
(check-equal? (list-set '(a b c d) 2 '(1 2))  '(a b (1 2) d))
(check-equal? (list-set '(a b c d) 3 '(1 5 10)) '(a b c (1 5 10)))