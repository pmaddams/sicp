#lang racket/base

; Exercise 3.71

(provide (all-defined-out))

(require racket/stream)

(define (weighted-pairs s1 s2 weight)
  (let loop ((s1 s1) (s2 s2))
    (let ((a (stream-first s1))
          (b (stream-first s2)))
      (stream-cons (list a b)
                   (merge-weighted (stream-map (lambda (b*) (list a b*))
                                               (stream-rest s2))
                                   (loop (stream-rest s1) (stream-rest s2))
                                   weight)))))

(define (merge-weighted s1 s2 weight)
  (let loop ((s1 s1) (s2 s2))
    (let ((a (stream-first s1))
          (b (stream-first s2)))
      (if (< (weight a) (weight b))
          (stream-cons a (loop (stream-rest s1) s2))
          (stream-cons b (loop s1 (stream-rest s2)))))))

(define (sum-of-cubes l)
  (apply + (map cube l)))

(define (cube n) (expt n 3))

(define ramanujan
  (let loop ((s (weighted-pairs (in-naturals 1) (in-naturals 1) sum-of-cubes)))
    (let* ((l1 (stream-first s))
           (n (sum-of-cubes l1))
           (l2 (stream-first (stream-rest s)))
           (m (sum-of-cubes l2)))
      (if (= n m)
          (stream-cons n (loop (stream-rest (stream-rest s))))
          (loop (stream-rest s))))))
