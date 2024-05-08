#lang eopl

(define-datatype diff-tree diff-tree?
  (one)
  (diff (lhs diff-tree?)
        (rhs diff-tree?)))

(define (integer->diff-tree n)
  (cond [(= n 1) (one)]
        [(> n 0) (successor (integer->diff-tree (- n 1)))]
        [else (predecessor (integer->diff-tree (+ n 1 )))]))

(define (diff-tree->integer n)
  (cases diff-tree n
    [one () 1]
    [diff (lhs rhs) (- (diff-tree->integer lhs)
                       (diff-tree->integer rhs))]))

(define (diff-tree=? n m)
  (= (diff-tree->integer n) (diff-tree->integer m)))

(define (zero) (diff (one) (one)))
(define (is-zero? n)
  (cases diff-tree n
    (one () #f)
    (diff (lhs rhs) (diff-tree=? lhs rhs))))

(define (successor n)
  (diff n (diff (zero) (one))))

(define (predecessor n)
  (diff n (one)))

(display  (diff-tree->integer (successor (integer->diff-tree 1))))

(define (diff-tree-plus m n)
  (diff m (diff (zero) n)))

(define (check-diff-tree=? m n)
  (diff-tree=? m n))

(check-diff-tree=? (diff-tree-plus (zero) (one))
                   (one))

(check-diff-tree=? (diff-tree-plus (integer->diff-tree 555)
                                   (integer->diff-tree 444))
                   (integer->diff-tree 111))