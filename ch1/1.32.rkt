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
                    (p (next a)
                       (combiner result (term a)))))))
    (p a null-value)))

(define accumulate accumulate-iter)

(define (sum term a next b)
  (accumulate + 0 term a next b))

(sum identity 1 inc 10)
;; 55

(define (product term a next b)
  (accumulate * 1 term a next b))

(product identity 1 inc 10)
;; 3628800
