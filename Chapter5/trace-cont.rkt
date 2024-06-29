#lang eopl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         the-lexical-spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-lexical-spec
  '(
    ;;white space rule
    [whitespace (whitespace) skip]
    ;;comment rule
    [comment ("%" (arbno (not #\newline))) skip]
    ;;identifier rule
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    ;;number rule
    [number (digit (arbno digit)) number]
    ;;negative number rule
    [number ("-" digit (arbno digit)) number]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         the-grammar-spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-grammar
  '(;;program non-terminal
    [program (expression) a-program]
    ;;const expression
    [expression (number) const-exp]
    ;;diff expression
    [expression ("-" "(" expression "," expression ")") diff-exp]
    ;;zero? expression
    [expression ("zero?" "(" expression ")") zero?-exp]
    ;;if expression
    [expression ("if" expression "then" expression "else" expression) if-exp]
    ;;var expression
    [expression (identifier) var-exp]
    ;;let expression
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    ;;proc expression
    [expression ("proc" "(" identifier ")" expression) proc-exp]
    ;;call expression
    [expression ("(" expression expression ")") call-exp]
    ;; letrec expression
    [expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp]
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
(define (lift v)
  (cond [(integer? v) (num-val v)]
        [(boolean? v) (bool-val v)]
        [(proc? v) (proc-val v)]
        [else (eopl:error 'lift "unknown value ~s" v)]))

(define (lower ev)
  (cases expval ev
    [num-val (v) v]
    [bool-val (b) b]
    [proc-val (p) p]
    [else (eopl:error 'lower "unknown expval ~s" ev)]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  ;; this is the addition for creating a recursive call
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
    ;;this is the addition for letrec
    (extend-env-rec (p-name b-var p-body senv)
                    (if (eqv? search-sym p-name)
                        (proc-val (procedure b-var p-body env)) ; creates a new closure! / for recursive call
                        (apply-env senv search-sym))) ;; otherwise look in the parent
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
;;         Continuation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype continuation continuation?
  [end-cont]
  [zero-cont (saved-cont continuation?)]
  [let-cont (var symbol?)
            (body expression?)
            (env environment?)
            (saved-cont continuation?)]
  [if-cont (then expression?)
           (else expression?)
           (env environment?)
           (saved-cont continuation?)]
  [call-cont (rand expression?)
             (env environment?)
             (saved-cont continuation?)]
  [arg-cont (prc expval?)
            (saved-cont continuation?)]
  [diff-fst-cont (e expression?)
                 (env environment?)
                 (saved-cont continuation?)]
  [diff-snd-cont (val expval?)
                 (saved-cont continuation?)])

(define (apply-cont cont val)
  (begin
    (display "Applying : ")
    (print-continuation-chain cont)      ; Print the chain
    (newline)                             ; Add a newline for clarity
  (cases continuation cont
    [end-cont () (begin
                   (eopl:printf "End of computation.~%")
                   val)]
    [zero-cont (s-cont) (apply-cont s-cont (lift (zero? (lower val))))]
    [let-cont  (var body env s-cont) (value-of/k body (extend-env var val env) s-cont)]
    [if-cont   (then else env s-cont) (if (lower val)
                                          (value-of/k then env s-cont)
                                          (value-of/k else env s-cont))]
    [call-cont (rand env s-cont) (let ([new-cont (arg-cont val s-cont)])
                                   (new-cont-created new-cont)
                                   (value-of/k rand env new-cont ))]
    [arg-cont (prc s-cont) (apply-procedure/k (lower prc) val s-cont)]
    [diff-fst-cont (e env s-cont) (let ([new-cont (diff-snd-cont val s-cont)])
                                    (new-cont-created new-cont)
                                    (value-of/k e env new-cont))]
    [diff-snd-cont (v s-cont) (apply-cont s-cont (lift (- (lower v) (lower val))))])))

(define (new-cont-created cont)
  (begin
    (display "Created : ")
    (print-continuation-chain cont)      ; Print the chain
    (newline)    ))

;; Function to print the continuation chain
(define (print-continuation-chain cont)
  (let loop ((c cont))
    (cond
      ((continuation? c)
       (begin 
         (display (continuation-name c)) ; Display continuation type
         (display " -> ")                 ; Arrow for chaining
         (loop (continuation-next c))))    ; Recurse with the next continuation
      (else
       (display "%")))))           ; End of chain


;; Helper to get the continuation name (you'll need to add this)
(define (continuation-name cont)
  (cases continuation cont
    [end-cont () 'end-cont]
    [zero-cont (_) 'zero-cont]
    [let-cont (var body env s-cont) 'let-cont]
    [if-cont (then else env s-cont) 'if-cont]
    [call-cont (rand env s-cont) 'call-cont]
    [arg-cont (prc s-cont) 'arg-cont]
    [diff-fst-cont (e env s-cont) 'diff-fst-cont]
    [diff-snd-cont (val s-cont) 'diff-snd-cont]))

;; Helper to get the next continuation (you'll need to add this)
(define (continuation-next cont)
  (cases continuation cont
    [end-cont () #f]                         ; No next continuation
    [zero-cont (s-cont) s-cont]
    [let-cont (var body env s-cont) s-cont]
    [if-cont (then else env s-cont) s-cont]
    [call-cont (rand env s-cont) s-cont]
    [arg-cont (prc s-cont) s-cont]
    [diff-fst-cont (e env s-cont) s-cont]
    [diff-snd-cont (val s-cont) s-cont]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         value-of
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (value-of-program pgm)
  (cases program pgm
    [a-program (exp)
               (value-of/k exp (init-env) (end-cont))]))

(define (value-of/k exp env cont)
  (cases expression exp
    ;; SIMPLE EXPRESSION - <<START>>
    ;;(value-of (const-exp n) e) = n
    [const-exp (num)
               (apply-cont cont (num-val num))]
    ;;(value-of (var-exp x) e) = (apply-env e x)
    [var-exp (var)
             (apply-cont cont (apply-env env var))]

    ;;proc-exp - this comes from the concrete syntax
    [proc-exp (var body)
              (apply-cont cont (proc-val
                                (procedure var body env)))]
    ;; SIMPLE EXPRESSION - <<END>>


    ;; COMPLEX EXPRESSION - <<START>>
    ;;letrec
    ;; the result of letrec-body is the result of the entire expression
    ;; remaining nothing to do, so whole expression can be evaluated in one cont
    [letrec-exp (p-name b-var p-body letrec-body)
                (value-of/k letrec-body
                            (extend-env-rec p-name b-var p-body env)
                            cont)]

    ;;zero?-exp
    [zero?-exp (e1)
               (let ([new-cont (zero-cont cont)])
                 (new-cont-created new-cont)
                 (value-of/k e1 env new-cont)
                 )]


    ;;let-exp
    [let-exp (v e1 body)
             (let ([new-cont (let-cont v body env cont)])
               (new-cont-created new-cont)
               (value-of/k e1 env new-cont))]

    ;;if-exp
    [if-exp (e1 e2 e3)
            (let ([new-cont (if-cont e2 e3 env cont)])
              (new-cont-created new-cont)
              (value-of/k e1 env new-cont))]

    ;;call-exp
    [call-exp (rator rand)
              (let ([new-cont (call-cont rand env cont) ])
                (new-cont-created new-cont)
                (value-of/k rator env new-cont ))
              ]
    
    ;;diff-spec
    [diff-exp (e1 e2)
              (let ([new-cont (diff-fst-cont e2 env cont)])
                (new-cont-created new-cont)
                (value-of/k e1 env new-cont)
                )
              ]

    ;; COMPLEX EXPRESSION - <<END>>    
    ))

;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure ;; variant
   ;;what it is composed of
   (var symbol?)
   (body expression?)
   (env environment?)))

(define (apply-procedure/k prc val cont)
  (cases proc prc
    (procedure (var body senv)
               (value-of/k body (extend-env var val senv) cont))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              TEST TOOLS     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (check-expect ans expected)
  (if (eqv? (lower ans) expected)
      "done"
      (eopl:error 'check-expect "Fail: Expected : ~s Actual:~s" expected (lower ans))))


(define (run string)
  (value-of-program (scan&parse string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            EXAMPLES  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-expect (run "letrec f(x) =  if zero? (x) then 0 else -((f -(x, 1)), -(0,x)) 
                    in (f 1)") 1)