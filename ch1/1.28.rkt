#lang racket/base

(module+ test
  (require rackunit))

(define (square n)
  (* n n))

(define (expmod b x m)
  (define (loop b x)
    (cond ((zero? x) 1)
          ((even? x) (modulo (square (loop b (/ x 2))) m))
          (else (modulo (* b (loop b (sub1 x))) m))))
  (loop b x))

(module+ test
  (for ((i (in-range 10)))
    (let ((b (random 100))
          (x (random 100))
          (m (random 100)))
      (check = (expmod b x m) (modulo (expt b x) m)))))
