#lang racket/base

; Exercise 3.62

(provide (all-defined-out))

(require racket/stream)

(define ((sin-approx terms) x)
  (eval-series sin-series x terms))

(define ((cos-approx terms) x)
  (eval-series cos-series x terms))

(define ((tan-approx terms) x)
  (eval-series tan-series x terms))

(define ((exp-approx terms) x)
  (eval-series exp-series x terms))

(define (eval-series s x terms)
  (for/sum ((term (in-stream (stream-take s terms)))
            (n (in-naturals)))
    (* term (expt x n))))

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
                      (mul (stream-cons a (infinite 0))
                           (stream-rest s2))))))

(define (div s1 s2)
  (let ((a (stream-first s1))
        (b (stream-first s2)))
    (letrec ((q (stream-cons (/ a b)
                             (mul (stream-cons (/ 1 b) (infinite 0))
                                  (sub (stream-rest s1)
                                       (mul q (stream-rest s2)))))))
      q)))

(define (integral s)
  (for/stream ((a (in-stream s))
               (b (in-naturals 1)))
    (/ a b)))

(define (negative s)
  (stream-map (lambda (n) (- n)) s))

(define (infinite n)
  (stream-cons n (infinite n)))

(define sin-series
  (stream-cons 0 (integral cos-series)))

(define cos-series
  (stream-cons 1 (negative (integral sin-series))))

(define tan-series
  (div sin-series cos-series))

(define exp-series
  (stream-cons 1 (integral exp-series)))
