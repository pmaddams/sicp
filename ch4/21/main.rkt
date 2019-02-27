#lang racket/base

; Exercise 4.21

(provide (all-defined-out))

(define (Y f)
  (let ((r (lambda (r*)
             (lambda (x)
               ((f (r* r*)) x)))))
    (r r)))

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
