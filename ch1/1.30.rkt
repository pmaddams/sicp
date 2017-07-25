#lang sicp

(define (sum term a next b)
  (letrec ((iter (lambda (a result)
                   (if (> a b)
                       result
                       (iter (next a) (+ (term a) result))))))
    (iter a 0)))