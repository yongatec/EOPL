#lang racket
(require racket/string)
(define N 16)
(define BASE (- N 1))
(define BIGITS "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define (find-char-index char str)
  (define (iter index str)
    (cond
      [(not (non-empty-string? str)) #f] ; Character not found, return false
      [(char=? char (string-ref str 0)) index] ; Character found, return its index
      [else (iter (+ index 1) (substring str 1 (string-length str)))])) ; Continue searching
  (iter 0 str))

(define zero '())
(define (is-zero? n) (empty? n))
(define (successor n)
  (cond [(is-zero? n) (list (string-ref BIGITS 1))]
        [(eqv? (first n) (string-ref BIGITS BASE))
         (cons (string-ref BIGITS 0) (successor (rest n)))]
        [else (let [(ind (find-char-index (first n) BIGITS))]
                (cons (string-ref BIGITS (+ 1 ind)) (rest n)))]))
(define (predecessor n)
  (cond [(is-zero? n) (error "Negative numbers are not supported!")]
        [(equal? n '(#\1)) zero]
        [(eqv? (first n) (string-ref BIGITS 0))
         (cons (string-ref BIGITS BASE) (predecessor (rest n)))]
        [else (let [(ind (find-char-index (first n) BIGITS))]
                (cons (string-ref BIGITS (- ind 1)) (rest n)))]))

(is-zero? '())
(successor '())
(successor (successor '(#\8)))
(successor '(#\F))
(predecessor '(#\2))
(predecessor '(#\1))

(define (add a b)
  (cond [(is-zero? a) b]
        [else (add (predecessor a) (successor b))]))

(add (successor '(#\4)) (successor '(#\5)))

(define (times a b)
  (cond [(or (is-zero? a) (is-zero? b)) zero]
        [else (add (times (predecessor a) b) b)]))

(times (successor '(#\3)) (successor '(#\5)))

(define (factorial n)
  (cond [(is-zero? n) (successor zero)]
        [else (times n (factorial (predecessor n)))]))

(factorial (successor '(#\6)))

#|
Conclusions:

1. The larger the argument is, the longer the execution time is.
    
    This is because of inherent time complexity of factorial.
    
2. The larger the base is, the shorter the execution time is.
    
    Because larger base takes fewer bigits to represent a given number.
|#