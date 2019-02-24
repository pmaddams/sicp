#lang racket/base

; Exercise 1.32

(provide (all-defined-out))

(require racket/generator
         racket/sequence)

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (accumulate f z term a next b)
  (sequence-fold f z (generate term a next b)))

(define (generate term a next b)
  (let ((g (infinite-generator (yield a) (set! a (next a))))
        (p (lambda (a) (> a b))))
    (sequence-map term (in-producer g p))))
