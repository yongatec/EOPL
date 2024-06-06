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
     ("minus" "(" expression ")")
     minus-exp)

    (expression
     ("+" "(" expression "," expression ")")
     add-exp)

    (expression
     ("*" "(" expression "," expression ")")
     mult-exp)

    (expression
     ("mod" "(" expression "," expression ")")
     mod-exp)

    (expression
     ("equal?" "(" expression "," expression ")")
     eq-exp)

    (expression
     ("greater?" "(" expression "," expression ")")
     gt-exp)

    (expression
     ("less?" "(" expression "," expression ")")
     lt-exp)

    (expression
     ("emptylist")
     empty-list-exp)

    (expression
     ("car" "(" expression ")")
     car-exp)

    (expression
     ("cdr" "(" expression ")")
     cdr-exp)

    (expression
     ("cons" "(" expression "," expression ")")
     cons-exp)

    (expression
     ("null?" "(" expression ")")
     null-exp)
    
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
;; - a list
(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (list-val
   (lst list?)))

(define (lift v)
  (cond [(number? v) (num-val v)]
        [(boolean? v) (bool-val v)]
        [(list? v) (list-val v)]
        [else (eopl:error 'to-expval "Looking for num/bool/list but found ~s" v)]))

(define (lower ev)
  (cases expval ev
    (num-val (v) v)
    (bool-val (b) b)
    (list-val (l) l)
    (else (eopl:error 'lower "Unexpected expval ~s" ev) )))

;;extractors
(define (expval->num v)
  (cases expval v
    [num-val (num) num]
    [else (expval-extractor-error 'num v)]))

(define (expval->bool v)
  (cases expval v
    [bool-val (bool) bool]
    [else (expval-extractor-error 'bool v)]))

(define (expval->list v)
  (cases expval v
    [list-val (lst) lst]
    [else (expval-extractor-error 'bool v)]))

(define (expval-extractor-error variant value)
  (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (sym symbol?)
   (val expval?)
   (env environment?)))

(define (apply-env env search-sym)
  (cases environment env
    (empty-env () (eopl:error 'apply-env "No binding for ~s" search-sym))
    (extend-env (sym val senv) (if (eqv? sym search-sym)
                                   val
                                   (apply-env senv search-sym)))))


;; initial enviroment of the program capturing free variables
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

    ;;minus-exp
    [minus-exp (e1)
               (let ([v (expval->num (value-of e1 env))])
                 (num-val (* -1 v)))]

    ;add-exp
    [add-exp (e1 e2)
             (let ([v1 (expval->num (value-of e1 env))]
                   [v2 (expval->num (value-of e2 env))])
               (num-val (+ v1 v2)))]

    ;mult-exp
    [mult-exp (e1 e2)
              (let ([v1 (expval->num (value-of e1 env))]
                    [v2 (expval->num (value-of e2 env))])
                (num-val (* v1 v2)))]

    ;mod-exp
    [mod-exp (e1 e2)
             (let ([v1 (expval->num (value-of e1 env))]
                   [v2 (expval->num (value-of e2 env))])
               (num-val (modulus v1 v2)))]

    ;eq-exp
    [eq-exp (e1 e2)
             (let ([v1 (expval->num (value-of e1 env))]
                   [v2 (expval->num (value-of e2 env))])
               (bool-val (= v1 v2)))]

    ;gt-exp
    [gt-exp (e1 e2)
             (let ([v1 (expval->num (value-of e1 env))]
                   [v2 (expval->num (value-of e2 env))])
               (bool-val (> v1 v2)))]

    ;lt-exp
    [lt-exp (e1 e2)
             (let ([v1 (expval->num (value-of e1 env))]
                   [v2 (expval->num (value-of e2 env))])
               (bool-val (< v1 v2)))]

    ;;empty-list-exp
    [empty-list-exp ()
                    (list-val '())]

    ;;car-exp
    [car-exp (e1)
             (let ([lst (expval->list (value-of e1 env))])
               (if (null? lst)
                   (eopl:error "no car to emptylist")
                   (car lst)))]

    ;;cdr-exp
    [cdr-exp (e1)
             (let ([lst (expval->list (value-of e1 env))])
               (if (null? lst)
                   (eopl:error "no cdr to emptylist")
                   (cdr lst)))]

    ;;cons-exp
    [cons-exp (e1 e2)
              (let ([val (value-of e1 env)]
                    [lst (expval->list (value-of e2 env))])
                (list-val (cons val lst)))]

    ;;null-exp
    [null-exp (e1)
              (let ([lst (expval->list (value-of e1 env))])
                (bool-val (null? lst)))]
    ))

(define (modulus v1 v2)
  (if (< v1 v2)
      v1
      (modulus (- v1 v2) v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              TEST TOOLS     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-expect ans expected)
  (equal? ans (lift expected)))


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

(check-expect (run "minus(-(minus(5),9))") 14)
(check-expect (run "+(5,4)") 9)
(check-expect (run "-(15,114)") -99)
(check-expect (run "mod(15,6)") 3)

(check-expect (run "equal?(-(5,1), 4)") #t)
(check-expect (run "equal?(-(5,1), 0)") #f)
(check-expect (run "greater?(-(5,1), 3)") #t)
(check-expect (run "greater?(-(5,1), 4)") #t)
(check-expect (run "less?(-(5,1), 5)") #t)
(check-expect (run "less?(-(5,1), 4)") #f)

(check-expect (run "emptylist") '())
(check-expect (run "cons(1, emptylist)") '(1))
(check-expect (run "cons(2 , cons(1, emptylist))") '(2 1))
(check-expect (run "cons (zero?(let x = 4
                                in -(4,x)), cons(2 , cons(1, emptylist)) )") '(#t 2 1))
(check-expect (run "cons(emptylist, cons(2 , cons(1, emptylist)))") '(() 2 1))
(check-expect (run "cons(cons(1, emptylist), cons(2 , cons(1, emptylist)))") '((1) 2 1))
(check-expect (run "let x= 4
                    in cons(x,
                             cons(cons(-(x,1),emptylist),
                              emptylist))") '(4 (3)))
