#lang sicp

(define zero
  (lambda (f)
    (lambda (x)
      x)))

(define (add1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(add1 zero)
(lambda (f)
  (lambda (x)
    (f ((zero f) x))))
(lambda (f)
  (lambda (x)
    (f ((lambda (x) x) x))))
(lambda (f)
  (lambda (x)
    (f x)))

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(add1 one)
(lambda (f)
  (lambda (x)
    (f ((one f) x))))
(lambda (f)
  (lambda (x)
    (f ((lambda (x) (f x)) x))))
(lambda (f)
  (lambda (x)
    (f (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (add n m)
  (lambda (f)
    (lambda (x)
      ((n f) ((m f) x)))))