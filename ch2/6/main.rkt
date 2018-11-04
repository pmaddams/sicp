#lang racket/base

; Exercise 2.6: Church numerals

(provide church-0 church-1 church-2 church-add1
         number->church church->number church-=
         church-add church-mul church-expt)

(require racket/function)

(define (church-0 f) identity)

(define (church-1 f) f)

(define (church-2 f)
  (compose f f))

(define (church-add1 c)
  (lambda (f)
    (compose f (c f))))

(define (number->church n)
  (lambda (f)
    (repeated f n)))

(define (repeated f n)
  (let loop ((n n) (acc identity))
    (if (zero? n)
        acc
        (loop (sub1 n) (compose f acc)))))

(define (church->number c)
  ((c add1) 0))

(define (church-= c1 c2)
  (= (church->number c1)
     (church->number c2)))

(define (church-add c1 c2)
  (lambda (f)
    (compose (c1 f) (c2 f))))

(define (church-mul c1 c2)
  (lambda (f)
    (c1 (c2 f))))

(define (church-expt c1 c2)
  (c2 c1))
