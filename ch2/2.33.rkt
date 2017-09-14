#lang sicp

(define (foldr proc init l)
  (letrec ((f (lambda (l)
                (if (null? l)
                    init
                    (proc (car l)
                          (f (cdr l)))))))
    (f l)))

(define (map proc l)
  (foldr (lambda (x y)
           (cons (proc x) y))
         '()
         l))

(define (append l1 l2)
  (foldr cons l2 l1))

(define (length l)
  (foldr (lambda (x y)
           (inc y))
         0
         l))

(define (square x)
  (expt x 2))

(define (displayln x)
  (display x)
  (newline))

(let ((l1 '(1 2 3))
      (l2 '(4 5 6 7)))
  (displayln (map square l1))
  (displayln (append l1 l2))
  (displayln (length l2)))
;; (1 4 9)
;; (1 2 3 4 5 6 7)
;; 4
