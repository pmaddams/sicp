#lang racket/base

; Exercise 2.4

(provide (all-defined-out))

(define ((cons x y) m)
  (m x
     y
     (lambda (z) (set! x z))
     (lambda (z) (set! y z))))

(define (car c)
  (c (lambda (x y sx sy) x)))

(define (cdr c)
  (c (lambda (x y sx sy) y)))

(define (set-car! c z)
  (c (lambda (x y sx sy) (sx z))))

(define (set-cdr! c z)
  (c (lambda (x y sx sy) (sy z))))

(define (equal? x y)
  (if (not (procedure? x))
      (eq? x y)
      (and (procedure? y)
           (equal? (car x) (car y))
           (equal? (cdr x) (cdr y)))))

(define (member x l)
  (cond ((null? l) #f)
        ((equal? (car l) x) l)
        (else (member x (cdr l)))))

(define (remove x l)
  (cond ((null? l) '())
        ((equal? (car l) x) (cdr l))
        (else (cons (car l) (remove x (cdr l))))))

(define (length l)
  (let ((f (lambda (x acc) (add1 acc))))
    (foldr f 0 l)))

(define (append l1 l2)
  (foldr cons l2 l1))

(define (reverse l)
  (let ((f (lambda (x acc) (append acc (cons x '())))))
    (foldr f '() l)))

(define (map g l)
  (let ((f (lambda (x acc) (cons (g x) acc))))
    (foldr f '() l)))

(define (filter p l)
  (let ((f (lambda (x acc) (if (p x) (cons x acc) acc))))
    (foldr f '() l)))

(define (sum l)
  (foldr + 0 l))

(define (product l)
  (foldr * 1 l))

(define (foldr f z l)
  (if (null? l)
      z
      (f (car l) (foldr f z (cdr l)))))
