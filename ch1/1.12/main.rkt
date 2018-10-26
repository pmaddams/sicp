#lang racket/base

(provide choose)

(module+ main
  (for ((n (in-range 10)))
    (for ((k (in-range (add1 n))))
      (printf "~a " (choose n k)))
    (newline)))

(define (choose n k)
  (if (or (zero? n) (zero? k) (= n k))
      1
      (+ (choose (sub1 n) (sub1 k))
         (choose (sub1 n) k))))
