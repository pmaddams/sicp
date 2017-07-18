#lang sicp

(define (f-rec n)
  (letrec ((f (lambda (n)
                (if (< n 3)
                    n
                    (+ (f (dec n))
                       (* 2 (f (- n 2)))
                       (* 3 (f (- n 3))))))))
    (f n)))

(define (f-iter n)
  (letrec ((next (lambda (a b c)
                (+ c
                   (* 2 b)
                   (* 3 a))))
           (f (lambda (a b c count)
                (if (= count 0)
                    a
                    (f b c (next a b c) (dec count))))))
    (f 0 1 2 n)))