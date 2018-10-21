#lang sicp

(define (average . args)
  (/ (apply + args)
     (length args)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (square x)
  (expt x 2))

(define (sqrt-old x)
  (let ((good-enough? (lambda (guess)
                        (< (abs (- (square guess) x))
                           0.001))))
    (letrec ((s (lambda (guess)
                  (if (good-enough? guess)
                      guess
                      (s (improve guess x))))))
      (s 1.0))))

(define (sqrt-new x)
  (let ((good-enough? (lambda (prev guess)
                        (< (abs (/ (- prev guess)
                                   guess))
                           0.001))))
    (letrec ((s (lambda (prev guess)
                  (if (good-enough? prev guess)
                      guess
                      (s guess (improve guess x))))))
      (s 0.0 1.0))))

;; For very large numbers, the old definition of sqrt does not terminate
;; because the limited precision of the machine means the difference of
;; two very large squares will never be less than the given threshold.

;; (sqrt-old 99999999999999999999999999999)
;; <does not terminate>

(sqrt-new 99999999999999999999999999999)
;; 316227766017107.6

;; For very small numbers, the old definition of sqrt yields inaccurate
;; results, because the good-enough? test only checks if the square of the
;; guess is within epsilon of x, and quickly returns #t.

(sqrt-old 0.000000000000000000000000001)
;; 0.03125

(sqrt-new 0.000000000000000000000000001)
;; 3.162277777577544e-14
