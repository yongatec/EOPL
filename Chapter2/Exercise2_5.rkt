#lang eopl
(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No binding for ~s" search-var))

(define (empty-env) '())
(define (extend-env var val env)
  (list (cons var val) env))
(define (apply-env var env)
  (cond [(null? env) (report-no-binding-found search-var)]
        [(eqv? (car (car env)) var) (cdr (car env))]
        [else (apply-env var (cdr env))]))