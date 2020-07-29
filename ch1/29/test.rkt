#lang racket/base

(require racket/dict
         rackunit
         "main.rkt")

(define derivatives
  `((,(lambda (x) 1) . ,(lambda (x) 0))
    (,(lambda (x) x) . ,(lambda (x) 1))
    (,(lambda (x) (* 3 x)) . ,(lambda (x) 3))
    (,(lambda (x) (expt x 3)) . ,(lambda (x) (* 3 (expt x 2))))))

(test-case
 "integration"
 (let ((step 0.001))
   (for (((f f-prime) (in-dict derivatives)))
     (for ((i (in-range 5)))
       (let* ((lo (random 50))
              (hi (random (add1 lo) 100))
              (definite-integral (- (f hi) (f lo)))
              (tolerance (* 0.01 (abs definite-integral))))
         (check-= definite-integral (simpson-integral f-prime lo hi step) tolerance)
         (check-= definite-integral (riemann-integral f-prime lo hi step) tolerance))))))
