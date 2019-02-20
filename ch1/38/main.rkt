#lang racket/base

; Exercise 1.38

(provide e-approx
         within?)

(define (e-approx %)
  (let ((n (lambda (i) 1))
        (d (lambda (i)
                 (let ((j (add1 i)))
                   (if (zero? (remainder j 3))
                       (* 2 (quotient j 3))
                       1)))))
    (+ 2.0 (cont-frac-approx % n d))))

(define (cont-frac-approx % n d)
  (let loop ((guess (cont-frac n d 1)) (k 2))
    (let ((next (cont-frac n d k)))
      (if ((within? %) guess next)
          next
          (loop next (* 2 k))))))

(define (cont-frac n d k)
  (let loop ((i k) (acc 0))
    (if (zero? i)
        acc
        (loop (sub1 i) (/ (n i) (+ (d i) acc))))))

(define (within? %)
  (lambda (guess next)
    (< (abs (/ (- next guess) guess))
       (/ % 100.0))))
