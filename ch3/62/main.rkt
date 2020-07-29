#lang racket/base

; Exercise 3.62

(provide (all-defined-out))

(require racket/function
         racket/stream)

(define (sin-approx %)
  (series-approx sin-series odd? %))

(define (cos-approx %)
  (series-approx cos-series even? %))

(define (tan-approx %)
  (series-approx tan-series odd? %))

(define (exp-approx %)
  (series-approx exp-series identity %))

(define ((series-approx s p %) x)
  (let ((s (for/stream ((a (in-stream s))
                        (n (in-naturals))
                        #:when (p n))
             (* a (expt x n)))))
    (if (zero? (stream-first s))
        0
        (let loop ((acc (stream-first s)) (s (stream-rest s)))
          (let ((next (+ acc (stream-first s))))
            (if ((within? %) acc next)
                next
                (loop next (stream-rest s))))))))

(define ((within? %) guess next)
  (let ((diff (abs (/ (- next guess)
                      guess))))
    (< diff (* 0.01 %))))

(define (add s1 s2)
  (for/stream ((a (in-stream s1))
               (b (in-stream s2)))
    (+ a b)))

(define (sub s1 s2)
  (for/stream ((a (in-stream s1))
               (b (in-stream s2)))
    (- a b)))

(define (mul s1 s2)
  (let ((a (stream-first s1))
        (b (stream-first s2)))
    (stream-cons (* a b)
                 (add (mul (stream-rest s1) s2)
                      (mul (stream-cons a zero)
                           (stream-rest s2))))))

(define (div s1 s2)
  (let ((a (stream-first s1))
        (b (stream-first s2)))
    (letrec ((q (stream-cons (/ a b)
                             (mul (stream-cons (/ 1 b) zero)
                                  (sub (stream-rest s1)
                                       (mul q (stream-rest s2)))))))
      q)))

(define (integral s)
  (for/stream ((a (in-stream s))
               (b (in-naturals 1)))
    (/ a b)))

(define (negative s)
  (stream-map (lambda (n) (- n)) s))

(define zero (stream-cons 0.0 zero))

(define sin-series
  (stream-cons 0.0 (integral cos-series)))

(define cos-series
  (stream-cons 1.0 (negative (integral sin-series))))

(define tan-series
  (div sin-series cos-series))

(define exp-series
  (stream-cons 1.0 (integral exp-series)))
