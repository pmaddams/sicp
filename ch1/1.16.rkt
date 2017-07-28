#lang sicp

(define (fast-expt-iter b n)
  (letrec ((square (lambda (x)
                     (expt x 2)))
           (f (lambda (a b n)
                (cond ((zero? n) a)
                      ((even? n) (f a (square b) (/ n 2)))
                      (else (f (* a b) b (dec n)))))))
    (f 1 b n)))

(let ((expt fast-expt-iter))
  (expt 2 64))
;; 18446744073709551616