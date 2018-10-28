#lang racket/base

(provide naive-integral simpson-integral)

(define (naive-integral f lo hi step)
  (* (sum f lo (lambda (n) (+ step n)) hi) step))

(define (sum f lo next hi)
  (let loop ((n lo) (acc 0))
    (if (> n hi)
        acc
        (loop (next n) (+ (f n) acc)))))

(define (simpson-integral f lo hi step)
  (* (/ step 3)
     (let loop ((n (+ step lo)) (k 1) (acc (f lo)))
       (if (>= n hi)
           (+ (f hi) acc)
           (let ((term (* (if (even? k) 2 4) (f n))))
             (loop (+ step n) (add1 k) (+ term acc)))))))
