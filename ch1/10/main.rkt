#lang racket/base

; Exercise 1.10

(provide (all-defined-out))

(define (A n m)
  (cond ((= m 0) 0)
        ((= n 0) (* 2 m))
        ((= m 1) 2)
        (else (A (sub1 n)
                 (A n (sub1 m))))))

(define (f n) (* 2 n))

(define (g n)
  (if (zero? n)
      0
      (expt 2 n)))

(define (h n)
  (if (zero? n)
      0
      (let loop ((m (add1 n)))
        (if (zero? m)
            0
            (expt 2 (loop (sub1 m)))))))
