#lang eopl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         the-lexical-spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-lexical-spec
  '(
    ;;white space rule
    (whitespace
     (whitespace)
     skip)
    ;;comment rule
    (comment
     ("%" (arbno (not #\newline)))
     skip)
    ;;identifier rule
    (identifier
     (letter (arbno
              (or letter digit "_" "-" "?")))
     symbol)
    ;;number rule
    (number
     (digit (arbno digit))
     number)

    ;;negative number rule
    (number
     ("-" digit (arbno digit))
     number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         the-grammar-spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-grammar
  '((program
     (expression)
     a-program)

    (expression
     (number)
     const-exp)

    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression
     (identifier)
     var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)

    (expression
     ("(" expression expression ")")
     call-exp)

    (expression
     ("letrec" identifier "(" identifier ")" "=" expression "in" expression)
     letrec-exp)
    ))



;;This creates the AST data structures
(sllgen:make-define-datatypes the-lexical-spec the-grammar)

;;This creates scanner and parser
;;Takes a string and outputs AST 
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define (show-the-datatypes)
  (sllgen:list-define-datatypes the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Expressed Values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;an expressed value is either
;; - a number
;; - a boolean
(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?)))

;;extractors
(define (expval->num v)
  (cases expval v
    [num-val (num) num]
    [else (expval-extractor-error 'num v)]))

(define (expval->bool v)
  (cases expval v
    [bool-val (bool) bool]
    [else (expval-extractor-error 'bool v)]))

(define (expval-extractor-error variant value)
  (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value))

(define (expval->proc v)
  (cases expval v
    [proc-val (proc) proc]
    [else (expval-extractor-error 'proc v)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (pname symbol?)
   (bvar symbol?)
   (body expression?)
   (saved-env environment?)))

(define (apply-env env search-sym)
  (cases environment env
    (empty-env ()
               (eopl:error 'apply-env "No binding for ~s" search-sym))
    (extend-env (var val senv)
                (if (eqv? var search-sym)
                    val
                    (apply-env senv search-sym)))
    (extend-env-rec (p-name b-var p-body senv)
                    (if (eqv? search-sym p-name)
                        (proc-val (procedure b-var p-body env))
                        (apply-env senv search-sym)))
    ))


(define init-env 
  (lambda ()
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         value-of
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (value-of-program pgm)
  (cases program pgm
    [a-program (exp)
               (value-of exp (init-env))]))

(define (value-of exp env)
  (cases expression exp
    ;;(value-of (const-exp n) e) = n
    [const-exp (num)
               (num-val num)]
    ;;(value-of (var-exp x) e) = (apply-env e x)
    [var-exp (var)
             (apply-env env var)]
    ;;diff-spec
    [diff-exp (e1 e2)
              (let ([v1 (value-of e1 env)]
                    [v2 (value-of e2 env)])
                (let ([n1 (expval->num v1)]
                      [n2 (expval->num v2)])
                  (num-val (- n1 n2))))]
    ;;zero?-exp
    [zero?-exp (e1)
               (let* ([v1 (value-of  e1 env)]
                      [n1 (expval->num v1)])
                 (if (zero? n1)
                     (bool-val #t)
                     (bool-val #f)))]

    ;;if-exp
    [if-exp (e1 e2 e3)
            (let ([v1 (value-of e1 env)])
              (if (expval->bool v1)
                  (value-of e2 env)
                  (value-of e3 env)))]

    ;;let-exp
    [let-exp (v e1 body)
             (let ([v1 (value-of e1 env)])
               (value-of body
                         (extend-env v v1 env)))]

    ;;proc-exp - this comes from the concrete syntax
    [proc-exp (var body)
              (proc-val ;; converts it to AST
               (procedure var body env))]

    ;;call-exp
    [call-exp (rator rand)
              (let ([proc (expval->proc (value-of rator env))]
                    [arg (value-of rand env)])
                (apply-procedure proc arg))]

    ;;letrec
    [letrec-exp (p-name b-var p-body letrec-body)
                (value-of letrec-body
                          (extend-env-rec p-name b-var p-body env))]
    
    ))

;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure ;; variant
   ;;what it is composed of
   (var symbol?)
   (body expression?)
   (env environment?)))

(define (apply-procedure prc val)
  (cases proc prc
    (procedure (var body senv)
               (value-of body (extend-env var val senv)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              TEST TOOLS     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (val->expval val)
  (cond [(number? val) (num-val val)]
        [(boolean? val) (bool-val val)]
        [(proc? val) (proc-val val)]
        [else (eopl:error 'val->expval 
                          "Can't convert sloppy value to expval: ~s"
                          val)]))

(define (check-expect ans expected)
  (equal? ans (val->expval expected)))


(define (run string)
  (value-of-program (scan&parse string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            EXAMPLES  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(check-expect (run "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)") -1)
(check-expect (run "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)") 12)

(check-expect (run "letrec f(x) = -(x,1) in (f 33)") 32)
(check-expect (run "letrec f(x) = if zero?(x)  then 0 else -((f -(x,1)), -2)
                           in (f 4)") 8)

(check-expect (run "let m = -5 
                        in letrec f(x) = if zero?(x) then 0 else -((f -(x,1)), m)
                                  in (f 4)") 20)

(check-expect (run "letrec even(odd) = proc(x) if zero?(x) then 1 else (odd -(x,1))
                           in letrec  odd(x)  = if zero?(x) then 0 else ((even odd) -(x,1))
                                      in (odd 13)") 1)