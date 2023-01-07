#lang racket/base

; Exercise 3.71

(provide (all-defined-out))

(require racket/stream)

(define (weighted-pairs st1 st2 w)
  (let ((a (stream-first st1))
        (b (stream-first st2)))
    (stream-cons (cons a b)
                 (merge-weighted
                  (stream-map (lambda (b*) (cons a b*)) (stream-rest st2))
                  (weighted-pairs (stream-rest st1) (stream-rest st2) w)
                  w))))

(define (merge-weighted st1 st2 w)
  (let ((a (stream-first st1))
        (b (stream-first st2)))
    (if (< (w a) (w b))
        (stream-cons a (merge-weighted (stream-rest st1) st2 w))
        (stream-cons b (merge-weighted st1 (stream-rest st2) w)))))

(define (sum-of-cubes p)
  (+ (cube (car p))
     (cube (cdr p))))

(define (cube n) (expt n 3))

(define ramanujan-numbers
  (let loop ((st (weighted-pairs (in-naturals 1) (in-naturals 1) sum-of-cubes)))
    (let* ((n (sum-of-cubes (stream-first st))))
      (if (= n (sum-of-cubes (stream-first (stream-rest st))))
          (stream-cons n (loop (stream-rest (stream-rest st))))
          (loop (stream-rest st))))))
