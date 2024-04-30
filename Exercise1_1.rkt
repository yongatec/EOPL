#lang racket
;; K = {3n + 2 | n ∈ N} - comprehension form of a set!
;; K = {2,5,8,...} - list form of a set!
;; Let's write a inductive definition to check whether n is an element of this set K
(define (in-K↓? n)
  (cond [(< n 2) #f] ;;reduction step requires this false case!
        [(= n 2) #t] ;;base case, it is lucky we hit here!
        [else (in-K↓? (- n 3))]));; this approach is top-down , the reduction step is (- n 3)

(in-K↓? 2)
(in-K↓? 3)
(in-K↓? 8)
(in-K↓? 11)
(in-K↓? 128181)
(in-K↓? 128381)

(displayln "=====================")

(define (in-K↑? n)
  (define (k-helper c)
    (cond [(> c n) #f] ;; inductive step requires this false case
          [(= c n) #t] ;; the true case where hit!
          [else (k-helper (+ c 3))])) ;; inductive step (+ c 3)
  (k-helper 2))

(in-K↑? 2)
(in-K↑? 3)
(in-K↑? 8)
(in-K↑? 11)
(in-K↑? 128181)
(in-K↑? 128381)

(displayln "=====================")

;; Q = {2n + 3m + 1 | n, m ∈ N}
;; Q = {1,3,4,...}
;; Let's write a inductive definition to check whether n is an element of this set Q
(define (in-Q↓? n)
  (cond [(< n 1) #f]
        [(= n 1) #t]
        [else (or (in-Q↓? (- n 2)) (in-Q↓? (- n 3)))]))

(in-Q↓? 1)
(in-Q↓? 2)
(in-Q↓? 3)
(in-Q↓? 4)
(in-Q↓? 5)
(in-Q↓? 81)   ; n= 10 m=20
(in-Q↓? 160)  ; n= 30 m=33 (60+99+1)

(displayln "=====================")

(define (in-Q↑? n)
  (define (q-helper c)
    (cond [(> c n) #f]
          [(= c n) #t]
          [else (or (q-helper (+ c 2)) (q-helper (+ c 3)))]))
  (q-helper 1))

(in-Q↑? 1)
(in-Q↑? 2)
(in-Q↑? 3)
(in-Q↑? 4)
(in-Q↑? 5)
(in-Q↑? 81)   ; n= 10 m=20
(in-Q↑? 160)  ; n= 30 m=33 (60+99+1)

(displayln "=====================")

;;W = {(n, 2n + 1) | n ∈ N}
;;W = {(0,1),(1,3),(2,5),...}
;;Let's write a inductive definition to check whether n is an element of this set K
(define (in-W↓? p)
  (cond [(or  (< (car p) 0) (< (cdr p) 1)) #f]
        [(and (= (car p) 0) (= (cdr p) 1)) #t]
        [else (in-W↓? (cons (- (car p) 1) (- (cdr p) 2)))]))

(in-W↓? (cons 0 1))
(in-W↓? (cons 1 3))
(in-W↓? (cons 0 2))
(in-W↓? (cons 2 5))
(in-W↓? (cons 3 5))

(displayln "=====================")

(define (in-W↑? p)
  (define (w-helper c)
    (cond [(or (> (car c) (car p)) (> (cdr c) (cdr p))) #f]
          [(and (= (car c) (car p)) (= (cdr c) (cdr p))) #t]
          [else (w-helper (cons (+ (car c) 1) (+ (cdr c) 2)))]))
  (w-helper (cons 0 1)))

(in-W↑? (cons 0 1))
(in-W↑? (cons 1 3))
(in-W↑? (cons 0 2))
(in-W↑? (cons 2 5))
(in-W↑? (cons 3 5))

(displayln "=====================")
;;E = {(n, n^2) | n ∈ N}
;;E = {(0,0),(1,1), (2,4), ... }
;; E = (m,k) is one of:
;;  - (0,0)
;;  - (m-1, k -2m + 1) 
;; Let's write a inductive definition to check whether n is an element of this set E

(define (in-E↓? p)
  (cond [(or (< (car p) 0) (< (cdr p) 0)) #f]
        [(and (= (car p) 0) (= (cdr p) 0)) #t]
        [else (in-E↓? (cons (- (car p) 1) (+ (- (cdr p) (* 2 (car p))) 1)))]))

(in-E↓? (cons 0 0))
(in-E↓? (cons 1 0))
(in-E↓? (cons 1 1))
(in-E↓? (cons 2 3))
(in-E↓? (cons 2 4))
(in-E↓? (cons 3 9))
(in-E↓? (cons 4 16))
(in-E↓? (cons 5 25))

(displayln "=====================")

(define (in-E↑? p)
  (define (e-helper c)
    (cond [(or  (> (car c) (car p)) (> (cdr c) (cdr p))) #f]
          [(and (= (car c) (car p)) (= (cdr c) (cdr p))) #t]
          [else (in-E↑? (cons (+ (car c) 1) (+ (cdr c) (* 2 (car c)) 1)))]))
  (e-helper (cons 0 0)))

(in-E↓? (cons 0 0))
(in-E↓? (cons 1 0))
(in-E↓? (cons 1 1))
(in-E↓? (cons 2 3))
(in-E↓? (cons 2 4))
(in-E↓? (cons 3 9))
(in-E↓? (cons 4 16))
(in-E↓? (cons 5 25))