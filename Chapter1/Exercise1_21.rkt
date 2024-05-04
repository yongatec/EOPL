#lang racket
(require rackunit)

;;3. Template

(define (product sos1 sos2)
(cond [(empty? sos1) empty]
      [(empty? sos2) empty]
      [else (append (map (lambda (y) (list (first sos1) y)) sos2)
                    (product (rest sos1) sos2))]))

(check-equal? (product '(a b c) '(x y)) '((a x) (a y) (b x) (b y) (c x) (c y)))