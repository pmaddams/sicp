#lang sicp

(define (cbrt x)
  (letrec ((improve (lambda (guess)
                      (/ (+ (/ x
                               (expt guess 2))
                            (* guess 2))
                         3)))
           (good-enough? (lambda (prev guess)
                           (< (abs (/ (- prev guess)
                                      guess))
                              0.001)))
           (cbrt-iter (lambda (prev guess)
                        (if (good-enough? prev guess)
                            guess
                            (cbrt-iter guess (improve guess))))))
    (cbrt-iter 0.0 1.0)))