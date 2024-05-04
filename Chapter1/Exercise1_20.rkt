#lang racket
(require rackunit)

;; SList is one of:
;; - empty
;; - (cons Sexp SList)

;; Sexp is one of;
;; - Symbol
;; - SList


;;Template
#|
(define (fn-for-sexp sexp)
 (cond [(symbol? sexp) (...)]
        [else (fn-for-slist sexp)]))

(define (fn-for-slist slist)
 (cond [(empty? slist) (...)]
       [else (... (fn-for-sexp (first slist))
                   (fn-for-slist (rest slist)))]))
|#

;;Code the body

(define (count-occurences--slist s slist)
  (cond [(empty? slist) 0]
        [else (+ (count-occurences--sexp s (first slist))
                 (count-occurences--slist s (rest slist)))]))

(define (count-occurences--sexp s sexp)
  (cond [(symbol? sexp) (if (eq? s sexp)
                            1
                            0)]
        [else (count-occurences--slist s sexp)]))


(define count-occurences count-occurences--slist)

(check-equal? (count-occurences 'x '((f x) y (((x z) x)))) 3)
(check-equal? (count-occurences 'x '((f x) y (((x z) () x)))) 3)
(check-equal? (count-occurences 'w '((f x) y (((x z) x)))) 0)
