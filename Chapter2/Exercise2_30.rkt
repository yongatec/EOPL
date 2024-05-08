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

(define (report-error expected datum)
    (eopl:error 'parse "Expect ~a, but got ~s." expected datum))

;'lambda can't be a variable
;(a b c), is a syntactic error in lc-exp

(define (cvar-exp var)
  (if (eqv? var 'lambda)
      (report-error "an idenitifier" "lambda")
      (var-exp var)))

(define (clambda-exp exp)
  (lambda-exp (cadr exp) (parse (caddr exp))))     

(define (capp-exp exp)
  (if (eqv? (car exp) 'lambda)
      (report-error "app-exp" exp)
      (app-exp (parse (car exp)) (map parse (cdr exp)))))

(define (parse exp)
  (cond [(symbol? exp) (cvar-exp exp)]
        [(and (eqv? (length exp) 3) (eqv? (car exp) 'lambda)) (clambda-exp exp)]
        [else (capp-exp exp)]))

;(display (parse '(lambda (f x y z) (+ (f (f x y) z) z))))
;(display (parse '(lambda (f x y z) (+ (f (f x y) z) (lambda)))))
(display (parse '(lambda x)))

