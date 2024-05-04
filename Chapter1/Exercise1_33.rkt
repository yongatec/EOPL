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
;(define (double-tree bt) 0)

#;
;;3. Template
(define (double-tree bt)
  (cond [(leaf? bt) (...)]
        [else (... (contents-of bt)
                   (double-tree (lbt bt))
                   (double-tree (rbt bt)))]))

;;4. Code the body
(define (double-tree bt)
  (cond [(leaf? bt) (leaf (* (contents-of bt) 2))]
        [else (interior-node (contents-of bt)
                             (double-tree (lbt bt))
                             (double-tree (rbt bt)))]))

;;2. Examples
(check-equal? (double-tree
               (interior-node 'red
                              (interior-node 'bar
                                             (leaf 26)
                                             (leaf 12))
                              (interior-node 'red
                                             (leaf 11)
                                             (interior-node 'quux
                                                            (leaf 117)
                                                            (leaf 14)))))
              (interior-node 'red
                             (interior-node 'bar
                                            (leaf (* 26 2))
                                            (leaf (* 12 2)))
                             (interior-node 'red
                                            (leaf (* 11 2))
                                            (interior-node 'quux
                                                           (leaf (* 117 2))
                                                           (leaf (* 14 2))))))
