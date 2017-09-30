#lang sicp

(define (square x)
  (* x x))

(define (expt b n)
  (letrec ((e (lambda (a b n)
                (cond ((zero? n) a)
                      ((even? n) (e a
                                    (square b)
                                    (/ n 2)))
                      (else (e (* a b)
                               b
                               (dec n)))))))
    (e 1 b n)))

(expt 2 64)
;; 18446744073709551616
