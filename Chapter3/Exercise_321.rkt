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
    
    ;;proc expression
    (expression
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)

    ;;application/ call expression 
    (expression
     ("(" expression (arbno expression) ")")
     call-exp)


    (expression
     ("letproc" identifier "(" (arbno identifier) ")" "=" expression "in" expression)
     letproc-exp)

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
  ;;creating a variant for procedure
  (proc-val
   (proc proc?)))

(define (lift v)
  (cond [(number? v) (num-val v)]
        [(boolean? v) (bool-val v)]
        [(proc? v) (proc-val v)]
        [else (eopl:error 'lift "Unexpected Value ~s" v)]))

(define (lower v)
  (cases expval v
    [num-val (n) n ]
    [bool-val (b) b]
    [proc-val (p) p]
    [else (eopl:error 'lower "Unexpected Value ~s" v)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val expval?)
   (env environment?)))


(define (apply-env env search-sym)
  (cases environment env
    (empty-env ()
               (eopl:error 'apply-env "No binding for ~s" search-sym))
    (extend-env (var val senv)
                (if (eqv? var search-sym)
                    val
                    (apply-env senv search-sym)))))

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
                (let ([n1 (lower v1)]
                      [n2 (lower v2)])
                  (lift (- n1 n2))))]
    ;;zero?-exp
    [zero?-exp (e1)
               (let* ([v1 (value-of  e1 env)]
                      [n1 (lower v1)])
                 (if (zero? n1)
                     (lift #t)
                     (lift #f)))]

    ;;if-exp
    [if-exp (e1 e2 e3)
            (let ([v1 (value-of e1 env)])
              (if (lower v1)
                  (value-of e2 env)
                  (value-of e3 env)))]

    ;;let-exp
    [let-exp (v e1 body)
             (let ([v1 (value-of e1 env)])
               (value-of body
                         (extend-env v v1 env)))]

    ;;proc-exp - this comes from the concrete syntax
    [proc-exp (varlst body)
              (proc-val ;; converts it to AST
               (procedure varlst body env))]

    ;;call-exp
    [call-exp (rator randlst)
              (let ([proc (lower (value-of rator env))]
                    [arglst (map (lambda (rand) (value-of rand env)) randlst)])
                (apply-procedure proc arglst))]

    [letproc-exp (fname pnames fbody lbody)
                 (value-of (let-exp fname (proc-exp pnames fbody) lbody) env)]
    
    ))

;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure ;; variant
   ;;what it is composed of
   (var list?)
   (body expression?)
   (env environment?)))

(define (apply-procedure prc arglst)
  (cases proc prc
    (procedure (varlst body senv)
               (letrec ([extend (lambda (plst alst env)
                                  (cond [(null? plst) env]
                                        [else (extend (cdr plst)
                                                      (cdr alst)
                                                      (extend-env (car plst)
                                                                  (car alst)
                                                                  env))]))])
                 (value-of body (extend varlst arglst senv))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              TEST TOOLS     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-expect ans expected)
  (if (equal? ans (lift expected))
      "done"
      (eopl:error 'check-expect "Test Failed, Expected : ~s Ans: ~s" expected (lower ans))))

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
(check-expect (run "if zero?(0) then 3 else 4") 3)
(check-expect (run "if zero?(1) then 3 else 4") 4)
(check-expect (run "if zero?(-(11,11)) then 3 else 4") 3)
(check-expect (run "if zero?(-(11,12)) then 3 else 4") 4)
(check-expect (run "let x = 4 in x") 4)
(check-expect (run "let x = -(4,5) in -(x,-1)") 0)
(check-expect (run "let x = 3 in let y = 4 in -(x,y)") -1)
(check-expect (run "let x = 3 in let x = 4 in x") 4)
(check-expect (run "let x = 3 in let x = -(x,1) in x") 2)
;;new tests
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

(check-expect (run "letproc f(x) =  -(x,-1)
                     in (f 5)") 6)

(check-expect (run "
letproc fix(f) =
            letproc d(x) = proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in letproc
    t4m(f) = proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)") 12)

(check-expect (run "let f = proc(x) proc(y) -(x , -(0,y))
                    in ((f 4) 5)") 9)

