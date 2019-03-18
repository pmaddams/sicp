#lang racket/base

; Exercise 1.46

(provide (all-defined-out))

(require racket/function)

(define ((nth-root-approx n %) x)
  (let ((damped (repeated average-damp (exact-ceiling (log n))))
        (f (lambda (y) (/ x (expt y (sub1 n))))))
    ((fixed-point-approx %) (damped f) 1.0)))

(define ((average-damp f) x) (average x (f x)))

(define ((fixed-point-approx %) f guess)
  ((improve f (within? %)) guess))

(define ((improve f good-enough?) guess)
  (let loop ((guess guess) (next (f guess)))
    (if (good-enough? guess next)
        next
        (loop next (f next)))))

(define (within? %)
  (lambda (guess next)
    (< (abs (/ (- next guess) guess))
       (/ % 100.0))))

(define exact-ceiling
  (compose inexact->exact ceiling))

(define (repeated f n)
  (for/fold ((acc identity))
            ((i (in-range n)))
    (compose f acc)))

(define (average . args)
  (/ (apply + args)
     (length args)))
