#lang racket
(require rackunit)

(define (number->bintree num)
  (cons `(,num () ()) '()))

(define (current-element bt)
  (car (car bt)))

(define (lbt bt)
  (cadr (car bt)))

(define (rbt bt)
  (caddr (car bt)))

(define (parent bt)
  (cdr bt))

(define (move-to-left-son bt)
  (let* ([current (car bt)]
         [value (car current)]
         [lson (cadr current)]
         [rson (caddr current)]
         [parent (cdr bt)])
    (cons lson
          (cons (list value 'right rson)
                parent))))

(define (move-to-right-son bt)
  (let* ([current (car bt)]
         [value (car current)]
         [lson (cadr current)]
         [rson (caddr current)]
         [parent (cdr bt)])
    (cons rson
          (cons (list value 'left lson)
                parent))))


(define (move-up bt)
   (let* ([current (car bt)]
         [value (car current)]
         [parent (cdr bt)])
     (cond [(empty? parent) bt]
           [else (let* ([ptr (car parent)]
                        [val (car ptr)]
                        [dir (cadr ptr)]
                        [son (caddr ptr)]
                        [gparent (cdr parent)])
                   (cond [(eqv? dir 'left) (cons `(,val ,son ,bt) gparent)]
                         [else (cons `(,val ,bt ,son) gparent)]))])))
(define (at-root? bt)
  (empty? (cdr bt)))

(define (at-leaf? bt)
  (null? (car bt)))

(define (insert-to-left num bt)
  (let* ([current (car bt)]
         [value (car current)]
         [lson (cadr current)]
         [rson (caddr current)]
         [parent (cdr bt)])
    (cons `(,value (,num ,lson ()) ,rson) parent)))

(define (insert-to-right num bt)
    (let* ([current (car bt)]
         [value (car current)]
         [lson (cadr current)]
         [rson (caddr current)]
         [parent (cdr bt)])
    (cons `(,value ,lson (,num () ,rson)) parent)))

(check-equal? (number->bintree 13) (cons '(13 () ()) '()))
(define t1 (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree 13))))
(check-equal? t1 (cons '(13 (12 () ())
                      (14 () ())) '()))

(check-equal? (move-to-left-son t1)
              (cons '(12 () ())
                    (cons '(13 right (14 () ())) '())))

(check-equal? (move-to-right-son t1)
              (cons '(14 () ())
                    (cons '(13 left (12 () ())) '())))

(define t2 (insert-to-right 15 t1))
(define t2_nl (insert-to-left 44 (move-to-right-son t2)))
(check-equal? (move-up t2_nl) (cons `(,(current-element t2)
                                      ,(lbt t2)
                                      ,t2_nl) (parent t2)))

