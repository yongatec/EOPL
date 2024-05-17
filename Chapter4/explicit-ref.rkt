#lang eopl
;;;;;;;;;;;;;;;; THE STORE ;;;;;;;;;;;;;

;; the-store: a Scheme variable containing the current state of the
;; store.  Initially set to a dummy variable.
(define the-store 'unitialized)

;;empty-store : () -> Sto
(define (empty-store)
  '())

;; initialize-store! : () -> Sto
;; usage: (initialize-store!) sets the-store to the empty-store
(define (initialize-store!)
  (set! the-store (empty-store)))

;; reference? : SchemeVal -> Bool
(define (reference? v)
  (integer? v))

;; newref : ExpVal -> Ref
(define (newref val)
  (let ([next-ref (length the-store)])
    (set! the-store
          (append the-store (list val)))
    next-ref))

;; deref : Ref -> ExpVal
(define (deref ref)
  (list-ref the-store ref))

;;setref! : Ref * ExpVal -> Unspecified
(define (setref! ref val)
  (define (report-invalid-reference ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref the-store))
  (define (set-inner! st ind)
    (cond [(null? st) (report-invalid-reference ind st)]
          [(zero? ind) (cons val (cdr st))]
          [else (cons (car st) (set-inner! (cdr st) (- ind 1)))]))
  (set-inner! the-store ref))

;; get-store-as-list : () -> Listof(List(Ref,Expval))
;; Exports the current state of the store as a scheme list.
;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
;;   where foo, bar, and baz are expvals.
(define (get-store-as-list)
  (define (indexed-list st n)
    (cond [(null? st) '()]
          [else (cons (list n (car st)) (indexed-list (cdr st) (+ n 1)))]))
  (indexed-list the-store 0))

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

    ;; now this is a multi-definition one
    (expression
     ("letrec"
      (arbno identifier "(" identifier ")" "=" expression)
      "in" expression)
     letrec-exp)
      
    ;; new for explicit-refs
    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)

    (expression
     ("newref" "(" expression ")")
     newref-exp)

    (expression
     ("deref" "(" expression ")")
     deref-exp)

    (expression
     ("setref" "(" expression "," expression ")")
     setref-exp)))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?))
  (ref-val
   (ref reference?))
  )

;;; extractors:

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v)))))

(define expval->ref
  (lambda (v)
    (cases expval v
      (ref-val (ref) ref)
      (else (expval-extractor-error 'reference v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvar symbol?)
   (body expression?)
   (env environment?)))
  
(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec*
   (proc-names (list-of symbol?))
   (b-vars (list-of symbol?))
   (proc-bodies (list-of expression?))
   (saved-env environment?)))


;; env->list : Env -> List
;; used for pretty-printing and debugging
(define (env->list env)
  (cases environment env
    [empty-env ()
               '()]
    [extend-env (var val senv)
                (cons (list var (expval->printable val)
                            (env->list senv)))]
    [extend-env-rec* (pnames vars pbodies senv)
                     (cons
                      (list 'letrec pnames '...)
                      (env->list senv))]))

(define (expval->printable val)
  (cases expval val
    [proc-val (p)
              (cases proc p
                [procedure (var body senv)
                           (list 'procedure var '... (env->list senv))])]
    [else val]))


;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;


;; init-env : () -> Env
;; usage: (init-env) = [i=1, v=5, x=10]
;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.
;; Page: 69
(define init-env 
  (lambda ()
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;
(define (apply-procedure p arg)
  (cases proc p
    [procedure (var body env)
               (value-of body (extend-env var arg env))]))

(define (apply-env env search-sym)
  (cases environment env
    [empty-env ()
               (eopl:error 'apply-env "No binding for ~s" search-sym)]
    [extend-env (bvar bval saved-env)
                (if (eqv? search-sym bvar)
                    bval
                    (apply-env saved-env search-sym))]
    [extend-env-rec* (p-names b-vars p-bodies saved-env)
                     (let ([ind-p (location search-sym p-names)])
                       (cond [(eqv? ind-p #f) (apply-env saved-env search-sym)]
                             [else (proc-val (procedure (list-ref b-vars ind-p)
                                                        (list-ref p-bodies ind-p)
                                                        env))]))]))

;; location : Sym * Listof(Sym) -> Maybe(Int)
;; (location sym syms) returns the location of sym in syms or #f is
;; sym is not in syms.  We can specify this as follows:
;; if (memv sym syms)
;;   then (list-ref syms (location sym syms)) = sym
;;   else (location sym syms) = #f
(define (location sym syms)
  (define (index-sym n syms)
    (cond [(null? syms) #f]
          [(eqv? sym (car syms)) n]
          [else (index-sym (+ n 1) (cdr syms))]))
  (index-sym 0 syms))


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
(define (value-of-program pgm)
  (initialize-store!)
  (cases program pgm
    [a-program (e)
               (value-of e (init-env))]))

(define (value-of exp env)
  (define (value-of-begins e es)
    (let ([v1 (value-of e env)])
      (if (null? es)
          v1
          (value-of-begins (car es) (cdr es)))))
  (cases expression exp
    [const-exp (num) (num-val num)]
    [var-exp (var) (apply-env env var)]
    [diff-exp (e1 e2)
              (let ([n1 (expval->num  (value-of e1 env))]
                    [n2 (expval->num  (value-of e2 env))])
                (num-val (- n1 n2)))]
    [zero?-exp (e)
               (let ([v (expval->num (value-of e env))])
                 (bool-val (zero? v)))]
    [if-exp (ce te ee)
            (let ([v (expval->bool (value-of ce env))])
              (if v
                  (value-of te env)
                  (value-of ee env)))]
    [let-exp (var val body)
             (value-of body (extend-env var (value-of val env) env))]

    [proc-exp (var body)
              (proc-val (procedure var body env))]

    [call-exp (rator rand)
              (let ([op (expval->proc (value-of rator env))]
                    [arg (value-of rand env)])
                (apply-procedure op arg))]

    [letrec-exp (p-names b-vars p-bodies letrec-body)
                (value-of letrec-body
                          (extend-env-rec* p-names b-vars p-bodies env))]

    [begin-exp (exp exps)
               (value-of-begins exp exps)]

    [newref-exp (exp)
                (let ([v (value-of exp env)])
                  (ref-val (newref v)))]

    [deref-exp (exp)
               (let ([r (expval->ref (value-of exp env))])
                 (deref r))]

    [setref-exp (e1 e2)
                (let* ([r (expval->ref (value-of e1 env))]
                       [v (value-of e2 env)])
                  (begin
                    (setref! r v)
                    (num-val 23)))]))

(define (run src)
  (value-of-program (scan&parse src)))

(define (check-expect val expected)
  (equal? (expval->num val) expected))

(check-expect (run "begin 1; 2; 3 end") 3)
(check-expect (run "let g = let counter = newref(0) 
         in proc (dummy) let d = setref(counter, -(deref(counter),-1))
                    in deref(counter)
in -((g 11),(g 22))") -1)

(check-expect (run "let x = newref(17) in deref(x)") 17)
(check-expect (run "let x = newref(17) 
                          in begin setref(x,27); deref(x) end") 27)

(check-expect (run "let g = let counter = newref(0) 
         in proc (dummy) begin
                           setref(counter, -(deref(counter),-1));
                           deref(counter)
                         end
 in -((g 11),(g 22))") -1)

(check-expect (run "
let x = newref(0)
in letrec even(d) = if zero?(deref(x)) 
                   then 1
                   else let d = setref(x, -(deref(x),1))
                        in (odd d)
          odd(d)  = if zero?(deref(x)) 
                   then 0
                   else let d = setref(x, -(deref(x),1))
                        in (even d)
   in let d = setref(x,13) in (odd -100)") 1)


(check-expect (run "
let x = newref(0)
in letrec even(d) = if zero?(deref(x)) 
                   then 1
                   else let d = setref(x, -(deref(x),1))
                        in (odd d)
          odd(d)  = if zero?(deref(x)) 
                   then 0
                   else let d = setref(x, -(deref(x),1))
                        in (even d)
   in let d = setref(x,13) in (odd -100)") 1)

(check-expect (run "
let x = newref(22)
in let f = proc (z) let zz = newref(-(z,deref(x))) in deref(zz)
   in -((f 66), (f 55))") 11)