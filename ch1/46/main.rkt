#lang racket/base

(provide nth-root-within
         repeated
         within?)

(define (nth-root-within percent n)
  (lambda (x)
    (let ((damped (repeated average-damp (ceiling (log n))))
          (f (lambda (y) (/ x (expt y (sub1 n))))))
      ((fixed-point-within percent) (damped f) 1.0))))

(define (repeated f n)
  (lambda (x)
    (let loop ((n n) (acc x))
      (if (zero? n)
          acc
          (loop (sub1 n) (f acc))))))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average . args)
  (/ (apply + args) (length args)))

(define (fixed-point-within percent)
  (lambda (f guess)
    ((improve f (within? percent)) guess)))

(define (improve better good-enough?)
  (lambda (guess)
    (let loop ((guess guess) (next (better guess)))
      (if (good-enough? guess next)
          next
          (loop next (better next))))))

(define (within? percent)
  (lambda (guess next)
    (< (abs (/ (- next guess) guess))
       (/ percent 100.0))))
