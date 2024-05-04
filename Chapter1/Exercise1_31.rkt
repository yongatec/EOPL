#lang racket
(require rackunit)

;BinTree ::= Int | (Symbol BinTree BinTree)

(define (leaf nv) nv)
(define (interior-node sym l r) (list sym l r))
(define (leaf? bt) (integer? bt))
(define (lbt bt) (cadr bt))
(define (rbt bt) (caddr bt))
(define (contents-of bt)
  (cond [(integer? bt) bt]
        [else (car bt)]))
