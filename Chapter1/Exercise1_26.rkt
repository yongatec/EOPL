#lang racket
(require rackunit)

(define (up-slist slist)
  (cond [(empty? slist) empty]
        ;;if the element is a listm then we append the rest that way, we eliminate the outer extra parentheses
        ;;append eliminates extra parens from other elements
        [(list? (first slist)) (append (first slist) (up-slist (rest slist)))]
        ;;if the element is not a list, we have to use it as is, as an argument to cons.
        [else (cons (first slist) (up-slist (rest slist)))]))

(check-equal? (up-slist '((1 2) (3 4))) '(1 2 3 4))
(check-equal? (up-slist '((1 (2)) 3)) '(1 (2) 3))

