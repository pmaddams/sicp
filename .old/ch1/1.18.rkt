#lang sicp

(define (fast-*-iter a b)
  (letrec ((double (lambda (x)
                     (* x 2)))
           (halve (lambda (x)
                    (/ x 2)))
           (f (lambda (x a b)
                (cond ((zero? b) x)
                      ((even? b) (f x (double a) (halve b)))
                      (else (f (+ x a) a (dec b)))))))
    (f 0 a b)))