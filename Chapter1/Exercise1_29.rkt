#lang racket
(require rackunit)
;;4. Code the body
(define (sort loi)
  (cond [(empty? loi) empty]
        [else (let [(pivot (first loi))
                    (roe (rest loi))]
                (append (sort (filter (lambda (e) (<= e pivot)) roe))
                        (list pivot)
                        (sort (filter (lambda (e) (> e pivot)) roe))))]))

;;2.Examples
(check-equal? (sort '(8 2 5 2 3)) '(2 2 3 5 8))

;;3. Template
#;
(define (sort loi)
  (cond [(empty? loi) (...)]
        [else (... (first loi)
                   (rest loi))]))

