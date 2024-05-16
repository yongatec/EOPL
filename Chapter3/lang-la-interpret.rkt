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
  [procedure
   ;; no need for bound var since we track them by indices now!
   (body expression?)
   (env nameless-environment?)])

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

(define (apply-procedure p arg)
  (cases proc p
    [procedure (body nenv)
               (value-of-expression body (extend-nameless-env nenv arg))]))

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


;; nameless-environment
(define-datatype nameless-environment nameless-environment?
  [empty-nameless-env]
  [nameless-env
   (val expval?)
   (nenv nameless-environment?)])

(define (extend-nameless-env env val)
  (nameless-env val env))


(define (apply-nameless-env env idx)
  (cases nameless-environment env
    [empty-nameless-env ()
                        (eopl:error 'apply-nameless-env "no binding for the ~s th element" idx)]
    [nameless-env (val nenv)
                  (if (zero? idx)
                      val
                      (apply-nameless-env nenv (- idx 1)))]))

(define (init-nameless-env)
  (nameless-env (num-val 1) ; was 'i
                (nameless-env (num-val 5) ; was 'v
                              (nameless-env (num-val 10) ; was 'x
                                            (empty-nameless-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;
(define (value-of-program prg env)
  (cases program prg
    [a-program (e) (value-of-expression e env)]))

(define (value-of-expression exp env)
  (cases expression exp
    [const-exp (num)
               (num-val num)]
    [diff-exp (e1 e2)
              (num-val (- (expval->num (value-of-expression e1 env))
                          (expval->num (value-of-expression e2 env))))]
    [zero?-exp (e)
               (let ([v (expval->num (value-of-expression e env))])
                 (bool-val (zero? v)))]
    [if-exp (ce te ee)
            (let ([cnd (expval->bool (value-of-expression ce env))])
              (if cnd
                  (value-of-expression te env)
                  (value-of-expression ee env)))]
    [call-exp (rator rand)
              (let ([proc (expval->proc (value-of-expression rator env))]
                    [arg (value-of-expression rand env)])
                (apply-procedure proc arg))]
    
    [nameless-var-exp (num)
                      (apply-nameless-env env num)]
    [nameless-let-exp (val body)
                      (value-of-expression body (extend-nameless-env env (value-of-expression val env)))]
    [nameless-proc-exp (body)
                       (proc-val (procedure body env))]
    [else (eopl:error 'value-of-expression "unwanted expression ~s" exp)]

    ))

;; test utilities
(define (translate src-code)
  (translation-of-program (scan&parse src-code)))

(define (run src-code)
  (value-of-program (translate src-code) (init-nameless-env)))


(define (check-expect expval expected)
  (equal? (expval->num expval) expected))


;;;;;;;;;;;;;;;;;;;;;;;tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-expect (run "11") 11)
(check-expect (run "-33") -33)
(check-expect (run "-(44,33)") 11)
(check-expect (run "-(-(44,33),22)") -11)
(check-expect (run "-(55,-(22,11))") 44)
(check-expect (run "x") 10)
(check-expect (run "-(x,1)") 9)
(check-expect (run "-(1,x)") -9)
;(check-expect (run "foo") error)
;(check-expect (run "-(x,foo)") error)
(check-expect (run "if zero?(0) then 3 else 4") 3)
(check-expect (run "if zero?(1) then 3 else 4") 4)
;(check-expect (run "-(zero?(0),1)") error)
;(check-expect (run "-(1,zero?(0))") error)
;(check-expect (run "if 1 then 2 else 3") error)
(check-expect (run "if zero?(-(11,11)) then 3 else 4") 3)
(check-expect (run "if zero?(-(11,12)) then 3 else 4") 4)
(check-expect (run "let x = 4 in x") 4)
(check-expect (run "let x = -(4,5) in -(x,-1)") 0)
(check-expect (run "let x = 3 in let y = 4 in -(x,y)") -1)
(check-expect (run "let x = 3 in let x = 4 in x") 4)
(check-expect (run "let x = 3 in let x = -(x,1) in x") 2)
(check-expect (run "(proc (x) -(x,1) 30)") 29)
(check-expect (run "let f = proc (x) -(x,1) in (f 30)") 29)
(check-expect (run "(proc(f)(f 30)  proc(x)-(x,1))") 29)
(check-expect (run "((proc (x) proc (y) -(x,y)  5) 6)") -1)
(check-expect (run "let f = proc(x)
                               proc (y) -(x,y)
                            in ((f -(10,5)) 6)") -1)
(check-expect (run "let fix =  proc (f)
                                 let d = proc (x)
                                            proc (z) ((f (x x)) z)
                                     in proc (n) ((f (d d)) n)
                        in let t4m = proc (f)
                                   proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
                           in let times4 = (fix t4m)
                                  in (times4 3)") 12)
