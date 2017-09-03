#lang sicp

(define (expt b n)
  (let ((square (lambda (x)
                  (* x x))))
    (letrec ((e (lambda (a b n)
                  (cond ((zero? n) a)
                        ((even? n) (e a
                                      (square b)
                                      (/ n 2)))
                        (else (e (* a b)
                                 b
                                 (dec n)))))))
      (e 1 b n))))

(expt 2 64)
;; 18446744073709551616
