#lang sicp

(define (accumulate-rec combiner null-value term a next b)
  (letrec ((p (lambda (a)
                (if (> a b)
                    null-value
                    (combiner (term a)
                       (p (next a)))))))
    (p a)))

(define (accumulate-iter combiner null-value term a next b)
  (letrec ((p (lambda (a result)
                (if (> a b)
                    result
                    (p (next a) (combiner (term a) result))))))
    (p a null-value)))

(define (sum term a next b)
  (let ((accumulate accumulate-iter))
    (accumulate + 0 term a next b)))

(define (product term a next b)
  (let ((accumulate accumulate-iter))
    (accumulate * 1 term a next b)))