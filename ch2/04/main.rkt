#lang racket/base

; Exercise 2.4

(provide (all-defined-out))

(define ((cons x y) f)
  (f x
     y
     (lambda (z) (set! x z))
     (lambda (z) (set! y z))))

(define (car p)
  (p (lambda (x y sx sy) x)))

(define (cdr p)
  (p (lambda (x y sx sy) y)))

(define (set-car! p z)
  (p (lambda (x y sx sy) (sx z))))

(define (set-cdr! p z)
  (p (lambda (x y sx sy) (sy z))))

(define (equal? x y)
  (if (procedure? x)
      (and (procedure? y)
           (equal? (car x) (car y))
           (equal? (cdr x) (cdr y)))
      (eq? x y)))

(define (member x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) l)
        (else (member x (cdr l)))))

(define (remove x l)
  (cond ((null? l) '())
        ((equal? x (car l)) (cdr l))
        (else (cons (car l) (remove x (cdr l))))))

(define (length l)
  (foldr (lambda (x acc)
           (add1 acc))
         0
         l))

(define (append l1 l2)
  (foldr cons l2 l1))

(define (reverse l)
  (foldr (lambda (x acc)
           (append acc (cons x '())))
         '()
         l))

(define (map f l)
  (foldr (lambda (x acc)
           (cons (f x) acc))
         '()
         l))

(define (filter p l)
  (foldr (lambda (x acc)
           (if (p x)
               (cons x acc)
               acc))
         '()
         l))

(define (sum l)
  (foldr + 0 l))

(define (product l)
  (foldr * 1 l))

(define (foldr f z l)
  (if (null? l)
      z
      (f (car l) (foldr f z (cdr l)))))
