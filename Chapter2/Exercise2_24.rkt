#lang eopl
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define (bintree-to-list bt)
  (cases bintree bt
    (leaf-node (num) `(leaf-node ,num))
    (interior-node (k l r)
                   `(interior-node
                     ,k
                     ,(bintree-to-list l)
                     ,(bintree-to-list r)))))

(display (bintree-to-list (interior-node 'a
                                (leaf-node 3)
                                (leaf-node 4))))