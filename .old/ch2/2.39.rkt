#lang sicp

(define (foldr proc init l)
  (letrec ((f (lambda (l)
                (if (null? l)
                    init
                    (proc (car l)
                          (f (cdr l)))))))
    (f l)))

(define (foldl proc init l)
  (letrec ((f (lambda (l result)
                (if (null? l)
                    result
                    (f (cdr l)
                       (proc (car l)
                             result))))))
    (f l init)))

(define (reverse-a l)
  (foldr (lambda (x y)
           (append y (list x)))
         '()
         l))

(define (reverse-b l)
  (foldl cons '() l))

(define (displayln x)
  (display x)
  (newline))

(let ((l '(1 2 3)))
  (displayln (reverse-a l))
  (displayln (reverse-b l)))
;; (3 2 1)
;; (3 2 1)
