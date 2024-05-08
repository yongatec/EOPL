#lang eopl
(define identifier? symbol?)
;; this defines the AST structure
(define-datatype lc-exp lc-exp?
  [var-exp
   (var identifier?)]
  [lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?)]
  [app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))])

;This is the concrete syntax in the form of a list
;'(lambda (f x y z) (+ (f (f x y) z) z)))
(define (parse datum)
  (cond [(identifier? datum)
         (var-exp datum)]
        [(eqv? (car datum) 'lambda)
         (lambda-exp (cadr datum) (parse (caddr datum)))]
        [else (app-exp (parse (car datum)) (map parse (cdr datum)))]))

(display (parse '(lambda (f x y z) (+ (f (f x y) z) z))))
(parse '(lambda () x))
