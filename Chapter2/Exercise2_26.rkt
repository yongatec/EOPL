#lang eopl
(define-datatype rb-tree rb-tree?
  (red-node
   (l rb-tree?)
   (r rb-tree?))
  (blue-node
   (st blue?))
  (leaf-node
   (val integer?)))

(define-datatype blue blue?
  (empty-bn)
  (list-bn
   (rbst rb-tree?)
   (bnl blue?)))

(define (count-red rbt)
  (define (bn-helper bn cnt)
    (cases blue bn
      [empty-bn
       () bn]
      [list-bn
       (rbst bnl)
       (list-bn (rbt-helper rbst cnt)
                (bn-helper bnl cnt))]))
  (define (rbt-helper tree cnt)
    (cases rb-tree tree
      [red-node
       (l r) (red-node (rbt-helper l (+ cnt 1))
                       (rbt-helper r (+ cnt 1)))]
      [blue-node
       (st) (blue-node (bn-helper st cnt))]
      [leaf-node
       (val) (leaf-node cnt)]))
  (rbt-helper rbt 0))

(define rbt (red-node
             (blue-node
              (list-bn
               (leaf-node 26)
               (list-bn
                (leaf-node 12)
                (list-bn
                 (leaf-node 33)
                 (empty-bn)))))
             (red-node 
              (blue-node (empty-bn))
              (red-node  (red-node (leaf-node 22)
                                   (leaf-node 35))
                         (red-node (blue-node (list-bn (red-node (leaf-node 11)
                                                                 (leaf-node 44))
                                                       (empty-bn)))
                                   (leaf-node 66))))))

(display (count-red rbt))