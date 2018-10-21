#lang sicp

(define (foldr proc init l)
  (letrec ((f (lambda (l)
                (if (null? l)
                    init
                    (proc (car l)
                          (f (cdr l)))))))
    (f l)))

(define (count-leaves t)
  (foldr +
         0
         (map (lambda (st)
                (if (not (pair? st))
                    1
                    (count-leaves st)))
              t)))

(define (displayln x)
  (display x)
  (newline))

(let ((t '((1 2) (3 4))))
  (displayln (count-leaves t))
  (displayln (count-leaves (list t t))))
;; 4
;; 8
