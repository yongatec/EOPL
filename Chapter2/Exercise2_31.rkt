#lang eopl
(define-datatype prefix-exp prefix-exp?
  [const-exp
   (num integer?)]
  [diff-exp
   (fstop prefix-exp?)
   (sndop prefix-exp?)])

(define (parse-pexp explist)
  (let ([head (car explist)]
        [tail (cdr explist)])
    (cond [(integer? head)
           (cons (const-exp head) tail)] ;parse until you can leave the rest untouched <parsed><intact>
          [(eqv? head '-) ;this operation need 2 operands
           (let* ([op1-tl1 (parse-pexp tail)] ;parses the first operand
                  [op1 (car op1-tl1)] ;parsed
                  [tl1 (cdr op1-tl1)] ;<intact>
                  [op2-tl2 (parse-pexp tl1)] ; parses the second operand
                  [op2 (car op2-tl2)] ;parsed
                  [tl2 (cdr op2-tl2)]);<intact>
             (cons (diff-exp op1 op2) tl2))]; <parsed><intact>
          [else (eopl:error "Unexpected expression: ~a" exp)])))

(define (parse-plist prefix-list)
  (cond [(null? prefix-list) '()]
        [(and (< (length prefix-list) 3)
              (not (integer? (car prefix-list))))
         (eopl:error 'parse-plist "Not a prefix-exp : ~s" prefix-list)]
        [else   (let* ([exp-rest (parse-pexp prefix-list)]
                       [fst  (car exp-rest)]
                       [rest (cdr exp-rest)])
                  (if (null? rest)
                      fst
                      (eopl:error 'parse-plist "Expect null after prefix-exp, but got: ~s." rest)))]))

;(display (parse-plist '(- - 3 2 - 4 - 12 7)))
;(display (parse-plist '(- 3 2)))
;(display (parse-plist '(3)))
;(display (parse-plist '(- 2 3 -)))
;(display (parse-plist '(-)))
;(display (parse-plist '(- 2)))
;(display (parse-plist '( 3 2)))