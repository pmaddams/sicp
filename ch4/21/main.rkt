#lang racket/base

; Exercise 4.21

(provide (all-defined-out))

(define (even? n)
  ((lambda (even? odd?)
     (even? even? odd? n))

   (lambda (even? odd? n) ; even?
     (if (zero? n)
         #t
         (odd? even? odd? (sub1 n))))

   (lambda (even? odd? n) ; odd?
     (if (zero? n)
         #f
         (even? even? odd? (sub1 n))))))

(define (Y lam)
  ((lambda (rec)
     (lambda (x)
       ((lam (rec rec)) x)))

   (lambda (rec)
     (lambda (x)
       ((lam (rec rec)) x)))))

(define factorial
  (Y (lambda (f)
       (lambda (n)
         (if (<= n 1)
             1
             (* n (f (sub1 n))))))))

(define fibonacci
  (Y (lambda (f)
       (lambda (n)
         (if (<= n 1)
             n
             (+ (f (- n 1))
                (f (- n 2))))))))
