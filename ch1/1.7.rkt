#lang sicp

(define (improve guess x)
  (let ((average (lambda args
                   (/ (apply + args)
                      (length args)))))
    (average guess (/ x guess))))

(define (sqrt-old x)
  (letrec ((good-enough? (lambda (guess)
                           (< (abs (- (expt guess 2)
                                      x))
                              0.001)))
           (sqrt-iter (lambda (guess)
                        (if (good-enough? guess)
                            guess
                            (sqrt-iter (improve guess x))))))
    (sqrt-iter 1.0)))

(define (sqrt-new x)
  (letrec ((good-enough? (lambda (prev guess)
                           (< (abs (/ (- prev guess)
                                      guess))
                              0.001)))
           (sqrt-iter (lambda (prev guess)
                        (if (good-enough? prev guess)
                            guess
                            (sqrt-iter guess (improve guess x))))))
    (sqrt-iter 0.0 1.0)))

;; For very large numbers, the old definition of sqrt never terminates
;; because the limited precision of the machine means the difference of
;; two very large squares will never be less than the given epsilon.

;; (sqrt-old 99999999999999999999999999999)
;; does not terminate

(sqrt-new 99999999999999999999999999999)
;; 316227766017107.6

;; For very small numbers, the old definition of sqrt yields inaccurate
;; results, because the good-enough? test only checks if the square of the
;; guess is within epsilon of x, and quickly returns #t.

(sqrt-old 0.000000000000000000000000001)
;; 0.03125

(sqrt-new 0.000000000000000000000000001)
;; 3.162277777577544e-14