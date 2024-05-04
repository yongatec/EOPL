#lang racket
(require rackunit)

;;1.SPS
;(define (path lfv bt) empty)

#;
;;3. Template
(define (path lfv bt)
  (cond [(empty bt) (...)]
        [else (... (contents-of bt)
                   (path lfv (lbt bt))
                   (path lfv (rbt bt)))]))

(define (contents-of bt) (car bt))
(define (rbt bt) (caddr bt))
(define (lbt bt) (cadr bt))

;4. Code the body
(define (path lfv bt)
  (define (helper bt rsf)
    (cond [(empty? bt) (append rsf '(notfound))]
          [else (cond [(= (contents-of bt) lfv) (append rsf '(found))]
                      [(> lfv (contents-of bt)) (helper (rbt bt) (append rsf '(right)))]
                      [(< lfv (contents-of bt)) (helper (lbt bt) (append rsf '(left)))])]))
  (helper bt empty))

;;2. Examples
(check-equal? (path 18 '(14 (7 () (12 () ()))
                            (26 (20 (17 () ())
                                    ())
                                (31 () ())))) '(right left left right notfound))
(check-equal? (path 17 '(14 (7 () (12 () ()))
                            (26 (20 (17 () ())
                                    ())
                                (31 () ())))) '(right left left found))