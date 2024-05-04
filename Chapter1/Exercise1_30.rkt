#lang racket
(require rackunit)

;;3.Template
#;
(define (sort/predicate pred lst)
  (cond [(empty? lst) (...)]
        [else (... pred
                   (first lst)
                   (rest lst))]))

;;4.Code the body
(define (sort/predicate pred lst)
  (cond [(empty? lst) empty]
        [else (let [(p (first lst))
                    (r (rest lst))]
                (append (sort/predicate pred (filter (lambda (x) (pred x p)) r))
                        (list p)
                        (sort/predicate pred (filter (lambda (x) (not (pred x p))) r))))]))

;;2.Examples
(check-equal? (sort/predicate < '(8 2 5 2 3)) '(2 2 3 5 8))
(check-equal? (sort/predicate > '(8 2 5 2 3)) '(8 5 3 2 2))


