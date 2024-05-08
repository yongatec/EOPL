#lang eopl

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define (max-interior bt)
  (define (max2 a b)
    (let ([va (car a)]
          [vb (car b)])
      (if (> va vb)
          a
          b)))
  (define (max3 a b c)
    (let ([max (max2 a b)])
      (if (> max c)
          max
          c)))

  (define (sum a b node)
    (cons (+ (car a) (car b)) node))

  (define (is-leaf? n)
    (cases bintree (cdr n)
      [leaf-node (num) #t]
      [else #f]))
  
  (define (sym-of n)
    (cases bintree (cdr n)
      [interior-node (k l r) k]
      [else #f]))

  (define (helper bt)
    (cases bintree bt
      (leaf-node (num)
                 (cons num bt))
      (interior-node (k l r)
                     (let* ([lv  (helper l)]
                            [rv  (helper r)]
                            [btv (sum lv rv bt) ])
                       (cond [(and (is-leaf? lv) (is-leaf? rv)) btv]
                             [(is-leaf? lv) (max2 btv rv)]
                             [(is-leaf? rv) (max2 btv lv)]
                             [else (max3 btv lv rv)])))))

  (sym-of (helper bt)))


(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))

(display (max-interior tree-2))

(display (max-interior tree-3))