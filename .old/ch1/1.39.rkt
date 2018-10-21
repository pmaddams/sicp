#lang sicp

(define (cont-frac n d k)
  (letrec ((c (lambda (i result)
                (if (zero? i)
                    result
                    (c (dec i)
                       (/ (n i)
                          (+ result (d i))))))))
    (c k 0)))

(define (tan-cf x k)
  (let ((n (lambda (i)
             (- (expt x 2))))
        (d (lambda (i)
             (dec (* 2 i)))))
    (/ (cont-frac n d k) (- 0.0 x))))

(tan 1)
;; 1.5574077246549023

(tan-cf 1 10)
;; 1.5574077246549023
