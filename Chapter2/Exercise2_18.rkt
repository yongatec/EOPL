#lang racket
(require rackunit)
(define (number->sequence n)
  (list n '() '()))

(define (current-element nis)
  (first nis))

(define (move-to-left nis)
  (match nis
    [(list num (list smaller ...) (list larger ...))
     (list (first smaller) (cdr smaller) (cons num larger))]))

(define (move-to-right nis)
  (match nis
    [(list num (list smaller ...) (list larger ...))
     (list (first larger) (cons num smaller) (cdr larger))]))

(define (insert-to-left num nis)
  (match nis
    [(list cn (list smaller ...) (list larger ...))
     (list cn (cons num smaller) larger)]))

(define (insert-to-right num nis)
  (match nis
    [(list cn (list smaller ...) (list larger ...))
     (list cn smaller (cons num larger))]))

(check-equal? (number->sequence 7) '(7 () ()))
(check-equal? (current-element '(6 (5 4 3 2 1) (7 8 9))) 6)
(check-equal? (move-to-left '(6 (5 4 3 2 1) (7 8 9))) '(5 (4 3 2 1) (6 7 8 9)))
(check-equal? (move-to-right '(6 (5 4 3 2 1) (7 8 9))) '(7 (6 5 4 3 2 1) (8 9)))
(check-equal? (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (13 5 4 3 2 1) (7 8 9)))
(check-equal? (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (5 4 3 2 1) (13 7 8 9)))
