#lang eopl
(define identifier? symbol?)

(define-datatype lc-exp lc-exp?
  [var-exp
   (var identifier?)]
  [lambda-exp
   (bound-var identifier?)
   (body lc-exp?)]
  [app-exp
   (rator lc-exp?)
   (rand lc-exp?)])

(define (unparse exp)
  (cases lc-exp exp
      [var-exp (var) (symbol->string var)]
      [lambda-exp (bv bd) (string-append "(lambda ("
                                         (symbol->string bv)
                                         ")"
                                         (unparse bd)
                                         ")")]
      [app-exp (rator rand) (string-append "("
                                           (unparse rator)
                                           " "
                                           (unparse rand)
                                           ")")]))

(define ast (lambda-exp 'x
                        (app-exp (var-exp 'f)
                                 (app-exp (var-exp 'f)
                                          (var-exp 'x)))))

(display (unparse ast))