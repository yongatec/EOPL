#lang racket
(require rackunit)

;;4. Code the body
(define (filter-in pred lst)
(cond [(empty? lst) empty]
       [else (if (pred (first lst))
                (cons (first lst) (filter-in pred (rest lst)))
                (filter-in pred (rest lst)))]))

;;2. Examples
(check-equal? (filter-in number? '(a 2 (1 3) b 7)) '(2 7))
(check-equal? (filter-in symbol? '(a (b c) 17 foo)) '(a foo))
#|
;;3. Template
(define (filter-in pred lst)
 (cond [(empty? lst) (...)]
       [else (... pred
                  (first lst)
                  (rest lst))]))
|#