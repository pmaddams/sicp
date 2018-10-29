#lang racket/base

(provide naive-integral simpson-integral)

(define (naive-integral f lo hi step)
  (* (sum f lo (lambda (n) (+ n step)) hi) step))

(define (sum f lo next hi)
  (let loop ((n lo) (acc 0))
    (if (> n hi)
        acc
        (loop (next n) (+ acc (f n))))))

(define (simpson-integral f lo hi step)
  (* (/ step 3)
     (let loop ((n (+ lo step)) (k 1) (acc (f lo)))
       (if (>= n hi)
           (+ acc (f hi))
           (let ((term (* (if (even? k) 2 4) (f n))))
             (loop (+ n step) (add1 k) (+ acc term)))))))
