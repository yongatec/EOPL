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

;;1.SPS
;(define (number-leaves bt) empty)


;;4. code the body
;; The trick is to create an accumulator between branches
;; This can be done via transferring the what has computed in the left branch to right branch!
;; we do this by a pair!!!
;; both arms of the cond returns a pair!! and the transfer happens by let*

(define (number-leaves bt)
  (define (helper bt n)
    (cond [(leaf? bt) (cons (leaf n) (+ n 1))]
          [else (let* [(lres (helper (lbt bt) n))
                       (rres (helper (rbt bt) (cdr lres)))]
                  (cons (interior-node (contents-of bt)
                                       (car lres)
                                       (car rres))
                        (cdr rres)))]))
  (car (helper bt 0)))


;;2.Examples
(check-equal? (number-leaves (interior-node 'foo
                                            (interior-node 'bar
                                                           (leaf 26)
                                                           (leaf 12))
                                            (interior-node 'baz
                                                           (leaf 11)
                                                           (interior-node 'quux
                                                                          (leaf 117)
                                                                          (leaf 14)))))
                             (interior-node 'foo
                                            (interior-node 'bar
                                                           (leaf 0)
                                                           (leaf 1))
                                            (interior-node 'baz
                                                           (leaf 2)
                                                           (interior-node 'quux
                                                                          (leaf 3)
                                                                          (leaf 4)))))