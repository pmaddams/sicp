#lang racket/base

(require racket/dict
         rackunit
         "main.rkt")

(define integrated
  `((,(lambda (x) 1) . ,(lambda (x) x))
    (,(lambda (x) x) . ,(lambda (x) (/ (expt x 2) 2)))
    (,(lambda (x) (* 3 x)) . ,(lambda (x) (* (/ 3 2) (expt x 2))))
    (,(lambda (x) (expt x 3)) . ,(lambda (x) (/ (expt x 4) 4)))))

(test-case
 "integration"
 (let ((step 0.001))
   (for (((f g) (in-dict integrated)))
     (for ((i (in-range 5)))
       (let* ((lo (random 50))
              (hi (random (add1 lo) 100))
              (definite (- (g hi) (g lo)))
              (tolerance (/ (abs definite) 100.0)))
         (check-= definite (simpson-integral f lo hi step) tolerance)
         (check-= definite (riemann-integral f lo hi step) tolerance))))))
