#lang racket
(require rackunit)

(define (bintree n lt rt)
  (list n lt rt))

(define (root bt)
  (first bt))

(define (lbt bt)
  (first (rest bt)))

(define (rbt bt)
  (first (rest (rest bt))))

(define (number->bintree n)
  (list n '() '()))

(define (insert-to-left num bt)
  (let [(lbtree (lbt bt))
        (rbtree (rbt bt))
        (rn (root bt))]
    (cond [(empty? lbtree)
           (bintree rn (number->bintree num) rbtree)]
          [else (bintree rn (bintree num lbtree '())
                         rbtree)])))

(define (insert-to-right num bt)
  (let [(lbtree (lbt bt))
        (rbtree (rbt bt))
        (rn (root bt))]
    (cond [(empty? rbtree)
           (bintree rn lbtree (number->bintree num))]
          [else
           (bintree rn lbtree
                    (bintree num '() rbtree))])))

(define (move-to-left bt) (lbt bt))
(define (move-to-right bt) (rbt bt))
(define (at-leaf? bt) (empty? bt))
(define (current-element bt) (root bt))


(check-equal? (number->bintree 13) '(13 () ()))
(define t1 (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree 13))))
(check-equal? t1 '(13 (12 () ())
                      (14 () ())))

(check-equal? (move-to-left t1) '(12 () ()))

(check-equal? (current-element (move-to-left t1)) 12)
(check-equal? (at-leaf? (move-to-right (move-to-left t1))) #t)
(check-equal? (insert-to-left 15 t1) '(13
                                       (15
                                        (12 () ())
                                        ())
                                       (14 () ())))