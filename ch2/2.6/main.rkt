#lang racket/base

(provide church-0 church-1 church-2 church-add1
         number->church church->number church-=
         church-add church-mul church-expt)

(require racket/function)

(define (church-0 f)
  (lambda (x) x))

(define (church-1 f)
  (lambda (x) (f x)))

(define (church-2 f)
  (lambda (x) (f (f x))))

(define (church-add1 c)
  (lambda (f)
    (lambda (x)
      (f ((c f) x)))))

(define (number->church n)
  ((repeated church-add1 n) church-0))

(define (repeated f n)
  (lambda (x)
    (let loop ((n n) (acc x))
      (if (zero? n)
          acc
          (loop (sub1 n) (f acc))))))

(define (church->number c)
  ((c add1) 0))

(define (church-= c1 c2)
  (= (church->number c1)
     (church->number c2)))

(define (church-add c1 c2)
  (lambda (f)
    (lambda (x)
      ((c1 f) ((c2 f) x)))))

(define (church-mul c1 c2)
  (lambda (f)
    (lambda (x)
      ((c1 (c2 f)) x))))

(define (church-expt c1 c2)
  (c2 c1))
