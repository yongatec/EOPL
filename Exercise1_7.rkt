#lang racket
(require rackunit)
(define (length lox)
   (cond [(empty? lox) 0]
         [else (+ 1 (length (rest lox)))]))

(define (printl lox)
 (cond [(empty? lox) ""]
       [(string-append (format " ~a " (first lox) ) (printl (rest lox)))]))

(define (n-th-element lox n)
  (let [(l (length lox))]
    (cond [(<= l n) (format "list '( ~a ) doesn't contain ~a elements, it only has ~a elements" (printl lox) (+ n 1) l)]
        [(= n 0) (car lox)]
        [else (n-th-element (rest lox) (- n 1))])))

;;2.Check Expects
(check-eq? (n-th-element '(1 2 3 4 5) 0) 1)
(check-eq? (n-th-element '(1 2 3 4 5) 1) 2)
(check-eq? (n-th-element '(1 2 3 4 5) 2) 3)
(check-eq? (n-th-element '(1 2 3 4 5) 3) 4)
(check-eq? (n-th-element '(1 2 3 4 5) 4) 5)
(check-equal? (n-th-element '(1 2 3 4 5) 5) "list '(  1  2  3  4  5  ) doesn't contain 6 elements, it only has 5 elements")



