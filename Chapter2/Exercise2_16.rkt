#lang racket
(require rackunit)

(define (var-exp var)
  ;back-tick creates a list by evaluating the identifiers preceeded by , (comma)
  `(var-exp ,var))

(define (lambda-exp bound-var body)
  `(lambda-exp ,bound-var ,body))

(define (app-exp rator rand)
  `(app-exp ,rator ,rand))

(define (var-exp? exp)
  (match exp
    [(list 'var-exp (? symbol?)) #t]
    [else #f]))

(define (lambda-exp? exp)
  (match exp
    [(list 'lambda-exp (? var-exp?) (? lc-exp?)) #t]
    [else #f]))

(define (app-exp? exp)
  (match exp
    [(list 'app-exp (? lc-exp?) (? lc-exp?)) #t]
    [else #f]))

(define (lc-exp? exp)
  (or (var-exp? exp)
      (lambda-exp? exp)
      (app-exp? exp)))

(define (var-exp->var exp)
  (match exp
    [(list 'var-exp var) var]))

(define (lambda-exp->bound-var exp)
  (match exp
    [(list 'lambda-exp bvar _) bvar]))

(define (lambda-exp->body exp)
  (match exp
    [(list 'lambda-exp _ body) body]))

(define (app-exp->rator exp)
  (match exp
    [(list 'app-exp rator _) rator]))

(define (app-exp->rand exp)
  (match exp
    [(list 'app-exp _ rand) rand]))

(define x (var-exp 'x))
(define f (var-exp 'f))
(define f_x (app-exp f x))
(define lambda_f (lambda-exp x f_x))

;;3.Check-expects
(check-equal? (var-exp? x) #t) 
(check-equal? (var-exp? f_x) #f) 
(check-equal? (app-exp? f_x) #t) 
(check-equal? (app-exp? x) #f)
(check-equal? (lambda-exp? lambda_f) #t)
(check-equal? (lambda-exp? f_x) #f) 
(check-equal? (lc-exp? x) #t)
(check-equal? (lc-exp? f_x) #t)
(check-equal? (lc-exp? lambda_f) #t)
(check-equal? (var-exp->var f) 'f) 
(check-equal? (lambda-exp->bound-var lambda_f) x)
(check-equal? (lambda-exp->body lambda_f) f_x)
(check-equal? (app-exp->rator f_x) f)
(check-equal? (app-exp->rand f_x) x)