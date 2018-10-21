#lang sicp

(define (double f)
  (lambda (x)
    (f (f x))))

(define (repeated f n)
  (if (= n 1)
      f
      (repeated (double f) (dec n))))

(define (average . args)
  (/ (apply + args)
     (length args)))

(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x)
      (average (f (- x dx))
               (f x)
               (f (+ x dx))))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

((n-fold-smooth cos 4) 0.0)
;; 0.9999999997333333

((n-fold-smooth sin 4) 0.0)
;; 0.0
