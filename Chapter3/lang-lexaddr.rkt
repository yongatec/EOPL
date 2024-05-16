#lang eopl
;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))
  
(define the-grammar
  '((program (expression) a-program)

    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
      
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)   

    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)

    (expression
     ("(" expression expression ")")
     call-exp)
    
    ;;;;;;;;;;;;;;;; nameless ;;;;;;;;;;;;;;;;
    (expression ("%nameless-var" number)
                nameless-var-exp)
    (expression
     ("%let" expression "in" expression)
     nameless-let-exp)

    (expression
     ("%lexproc" expression)
     nameless-proc-exp)
    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
(define-datatype proc proc?
  [procedure])

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?)))


;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

(define (translation-of-program pgm)
  (cases program pgm
    [a-program (e)
               (a-program (translation-of e (init-senv)))]))


;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;
;; empty-senv : () -> Senv
(define empty-senv
  (lambda ()
    '()))

;; extend-senv : Var * Senv -> Senv
(define extend-senv
  (lambda (var senv)
    (cons var senv)))
  
;; apply-senv : Senv * Var -> Lexaddr
(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv) (report-unbound-var var))
      ((eqv? var (car senv))
       0)
      (else
       (+ 1 (apply-senv (cdr senv) var))))))

(define report-unbound-var
  (lambda (var)
    (eopl:error 'translation-of "unbound variable in code: ~s" var)))

;; init-senv : () -> Senv
(define init-senv
  (lambda ()
    (extend-senv 'i
                 (extend-senv 'v
                              (extend-senv 'x
                                           (empty-senv))))))
  

;; translation to lexical-address-language
;; Exp * Senv -> Nameless-exp
(define (translation-of exp senv)
  (cases expression exp
    [const-exp (num)
               (const-exp num)]
    [diff-exp (e1 e2)
              (diff-exp (translation-of e1 senv)
                        (translation-of e2 senv))]

    [zero?-exp (e)
               (zero?-exp (translation-of e senv))]
    [if-exp (ce te ee)
            (if-exp (translation-of ce senv)
                    (translation-of te senv)
                    (translation-of ee senv))]
    [var-exp (var)
             (nameless-var-exp (apply-senv senv var))]
    
    [let-exp (var exp body)
             (nameless-let-exp
              (translation-of exp senv)
              (translation-of body (extend-senv var senv)))]

    [proc-exp (var body)
              (nameless-proc-exp
               (translation-of body (extend-senv var senv)))]

    [call-exp (rator rand)
              (call-exp (translation-of rator senv)
                        (translation-of rand senv))]

    [else (report-invalid-source-expression exp)]))

(define (report-invalid-source-expression exp)
  (eopl:error 'value-of 
              "Illegal expression in source code: ~s" exp))

;; test utilities
(define (translate src-code)
  (translation-of-program (scan&parse src-code)))

;(display (translate "x"))
;(display (translate "-(x,1)"))
;(display (translate "let x = 3 in x"))
;(display (translate "let x = -(4,1) in -(x,1)"))
;(display (translate "let x = 3 in let x = -(x,1) in x"))
;(display (translate "(proc(x) -(x,1)  30)"))
;(display (translate "let f = proc (x) -(x,1) in (f 30)"))
;(display (translate "(proc(f)(f 30)  proc(x)-(x,1))"))
;(display (translate "((proc (x) proc (y) -(x,y)  5) 6)"))
;(display (translate "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)"))

(display (translate "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)"))