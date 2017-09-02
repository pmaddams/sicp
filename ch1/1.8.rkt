#lang sicp

(define (cbrt x)
  (let* ((square (lambda (x)
                   (expt x 2)))
         (improve (lambda (guess)
                    (/ (+ (/ x (square guess))
                          (* guess 2))
                       3)))
         (good-enough? (lambda (prev guess)
                         (< (abs (/ (- prev guess) guess))
                            0.001))))
    (letrec ((c (lambda (prev guess)
                  (if (good-enough? prev guess)
                      guess
                      (c guess (improve guess))))))
      (c 0.0 1.0))))

(cbrt 8)
;; 2.000000000012062
