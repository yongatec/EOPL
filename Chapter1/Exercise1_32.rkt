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
;(define (mark-leaves-with-red-depth bt) 0)

;;3.Template
#;
(define (mark-leaves-with-red-depth bt)
  (cond [(integer? bt) (...)]
        [else (... (contents-of bt)
                   (mark-leaves-with-red-depth (lbt bt))
                   (mark-leaves-with-red-depth (lbt rt)))]))

;;4. Code the body
(define (mark-leaves-with-red-depth bt)
  (define (helper bt cnt)
    (cond [(leaf? bt) (leaf cnt)]
          [else (if (eq? (contents-of bt) 'red)
                    (interior-node 'red (helper (lbt bt) (+ cnt 1)) (helper (rbt bt) (+ cnt 1)))
                    (interior-node (contents-of bt) (helper (lbt bt) cnt) (helper (rbt bt) cnt)))]))
  (helper bt 0))

;;2. Examples
(check-equal? (mark-leaves-with-red-depth
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
                                            (leaf 1)
                                            (leaf 1))
                             (interior-node 'red
                                            (leaf 2)
                                            (interior-node 'quux
                                                           (leaf 2)
                                                           (leaf 2)))))
