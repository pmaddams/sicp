#lang sicp

(define (sum term a next b)
  (letrec ((s (lambda (a result)
                (if (> a b)
                    result
                    (s (next a)
                       (+ result (term a)))))))
    (s a 0)))

(sum identity 0 inc 10)
;; 55
