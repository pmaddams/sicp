#lang racket/base

; Exercise 3.71

(provide (all-defined-out))

(require racket/stream)

(define (weighted-pairs st1 st2 weight)
  (let loop ((st1 st1) (st2 st2))
    (let ((a (stream-first st1))
          (b (stream-first st2)))
      (stream-cons (list a b)
                   (merge-weighted (stream-map (lambda (b*) (list a b*))
                                               (stream-rest st2))
                                   (loop (stream-rest st1) (stream-rest st2))
                                   weight)))))

(define (merge-weighted st1 st2 weight)
  (let loop ((st1 st1) (st2 st2))
    (let ((a (stream-first st1))
          (b (stream-first st2)))
      (if (< (weight a) (weight b))
          (stream-cons a (loop (stream-rest st1) st2))
          (stream-cons b (loop st1 (stream-rest st2)))))))

(define (sum-of-cubes l)
  (apply + (map cube l)))

(define (cube n) (expt n 3))

(define ramanujan
  (let loop ((st (weighted-pairs (in-naturals 1) (in-naturals 1) sum-of-cubes)))
    (let* ((l1 (stream-first st))
           (n (sum-of-cubes l1))
           (l2 (stream-first (stream-rest st)))
           (m (sum-of-cubes l2)))
      (if (= n m)
          (stream-cons n (loop (stream-rest (stream-rest st))))
          (loop (stream-rest st))))))
