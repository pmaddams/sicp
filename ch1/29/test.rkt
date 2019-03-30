#lang racket/base

(require racket/dict
         rackunit
         "main.rkt")

(define integrated
  `((,(lambda (x) 1) . ,(lambda (x) x))
    (,(lambda (x) x) . ,(lambda (x) (/ (expt x 2) 2)))
    (,(lambda (x) (/ (expt x 2) 2)) . ,(lambda (x) (/ (expt x 3) 6)))))

(test-case
 "integration"
 (let ((lo 1) (hi 10))
   (for (((f g) (in-dict integrated)))
     (for ((step (in-list '(0.01 0.001))))
       (let* ((definite (- (g hi) (g lo)))
              (tolerance (max step (/ definite 100))))
         (check-= definite (simpson-integral f lo hi step) tolerance)
         (check-= definite (riemann-integral f lo hi step) tolerance))))))
