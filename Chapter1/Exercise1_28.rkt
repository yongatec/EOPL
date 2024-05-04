#lang racket
(require rackunit)

;;1.SPS
;; Signature : ListOfInt ListOfInt -> ListOfInt
;; Purpose: merge 2 lists and return an ascending order list
;; Stub
;(define (merge loi1 loi2) empty)

;;3.Template
;;      loi1 / loi2              Empty               (cons Int ListOfInt)
;;Empty                          empty                  loi2 
;;(cons Int ListOfInt)           loi1                   (compare and cons)

;;4.Code the body
(define (merge loi1 loi2)
  (cond [(empty? loi1) loi2]
        [(empty? loi2) loi1]
        [else (if (< (first loi1) (first loi2))
                  (cons (first loi1) (merge (rest loi1) loi2))
                  (cons (first loi2) (merge loi1 (rest loi2))))]))

;;2.Examples
(check-equal? (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
(check-equal? (merge '(35 62 81 90 91) '(3 83 85 90)) '(3 35 62 81 83 85 90 90 91))