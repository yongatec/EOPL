#lang racket
(require rackunit)

; SList is one of:
; -empty
; (cons Sexp SList)

;Sexp is one of:
; -Symbol
; -SList

;;1.SPS
;; Signature : Symbol Symbol SList -> SList
;; Purpose: swaps the 2 symbols in the SList and returns a new SList
;; Stub

;(define (swapper old new slist) empty)

;;4. Code the body
(define (symswap olds news sym)
 (cond [(eq? olds sym) news]
       [(eq? news sym) olds]
       [else sym]))

(define (swapper-sexp olds news sexp)
  (cond [(symbol? sexp) (symswap olds news sexp)]
        [else (swapper-slist olds news sexp)]))

(define (swapper-slist olds news slist)
 (cond [(empty? slist) empty]
       [else (cons (swapper-sexp olds news (first slist))
                     (swapper-slist olds news (rest slist)))]))

(define swapper swapper-slist)

;;2.Examples
(check-equal? (swapper 'a 'd '(a b c d)) '(d b c a))
(check-equal? (swapper 'a 'd '(a d () c d)) '(d a () c a))
(check-equal? (swapper 'x 'y '((x) y (z (x)))) '((y) x (z (y))))

;;3. Template
#|
(define (fn-for-sexp sexp)
 (cond [(symbol? sexp) (...)]
       [else (fn-for-slist sexp)]))

(define (fn-for-slist slist)
 (cond [(empty? slist) (...)]
       [else (... (fn-for-sexp (first slist)
                  (fn-for-slist (rest slist))))]))
|#

