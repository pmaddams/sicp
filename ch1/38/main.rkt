#lang racket/base

; Exercise 1.38

(provide (all-defined-out))

(define (e-approx %)
  (let ((n (lambda (i) 1))
        (d (lambda (i)
             (let ((j (add1 i)))
               (if (zero? (remainder j 3))
                   (* 2 (quotient j 3))
                   1)))))
    (+ 2.0 (cont-frac-approx n d %))))

(define (cont-frac-approx n d %)
  (let loop ((guess (cont-frac n d 1)) (k 2))
    (let ((next (cont-frac n d k)))
      (if ((within? %) guess next)
          next
          (loop next (* 2 k))))))

(define (cont-frac n d k)
  (for/fold ((acc 0))
            ((i (in-range k 0 -1)))
    (/ (n i) (+ acc (d i)))))

(define ((within? %) guess next)
  (let ((diff (abs (/ (- next guess)
                      guess))))
    (< diff (* 0.01 %))))
