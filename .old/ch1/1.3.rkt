#lang sicp

(define (square x)
  (expt x 2))

(define (larger-two x y z)
  (let ((smallest (min x y z)))
    (cond ((= x smallest) (cons y z))
          ((= y smallest) (cons x z))
          (else (cons x y)))))

(define (sum-of-two-larger-squares x y z)
  (let* ((p (larger-two x y z))
         (a (car p))
         (b (cdr p)))
    (+ (square a)
       (square b))))

(sum-of-two-larger-squares 2 3 4)
;; 25
