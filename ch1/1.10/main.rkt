#lang racket/base

(provide A f g h)

(module+ main
  (for ((n (in-range 4)))
    (for ((m (in-range 4)))
      (printf "(A ~a ~a) -> ~a\n" n m (A n m)))))

(define (A n m)
  (cond ((= m 0) 0)
        ((= n 0) (* 2 m))
        ((= m 1) 2)
        (else (A (sub1 n)
                 (A n (sub1 m))))))

(define (f n)
  (* 2 n))

(define (g n)
  (if (zero? n)
      0
      (expt 2 n)))

(define (h n)
  (let loop ((n (if (zero? n) 0 (add1 n))))
    (if (zero? n)
        0
        (expt 2 (loop (sub1 n))))))