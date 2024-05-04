#lang racket
(require rackunit)

;(define (flatten lst) empty)

;; SInt is one of:
;; - SInt
;; - SList

;;SList is one of:
;; - empty
;; - (cons SInt SList)
;(1 2 (3 4 5 (5 7)) 6)
#|
;;3.Templates
(define (fn-for-sint sint)
 (cond [(integer? sint) (...)]
       [else (fn-for-slist sint)]))

(define (fn-for-slist slist)
 (cond [(empty? slist) (...)]
       [else (... (fn-for-sint (first slist))
                  (fn-for-slist (rest slist)))]))
|#

;;4. Code the body
(define (flatten-sint sint)
  (cond [(not (list? sint)) (list sint)]
        [else (flatten sint)]))

(define (flatten slist)
  (cond [(empty? slist) empty]
        [else (append (flatten-sint (first slist)) (flatten (rest slist)))]))

(check-equal? (flatten '((1 2) (3 4))) '(1 2 3 4))
(check-equal? (flatten '((1 (2)) 3)) '(1 2 3))
(check-equal? (flatten '(a b c)) '(a b c))
(check-equal? (flatten '((a) () (b ()) () (c))) '(a b c))
(check-equal? (flatten '((a b) c (((d)) e))) '(a b c d e))
(check-equal? (flatten '(a b (() (c)))) '(a b c))




