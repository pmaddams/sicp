#lang racket/base

; Exercise 1.29

(provide simpson-integral
         riemann-integral)

(require racket/sequence)

(define (simpson-integral f lo hi step)
  (* (/ step 3)
     (let loop ((x (+ lo step)) (i 1) (acc (f lo)))
       (if (>= x hi)
           (+ acc (f x))
           (let* ((coeff (if (even? i) 2 4))
                  (term (* coeff (f x))))
             (loop (+ x step) (add1 i) (+ acc term)))))))

(define (riemann-integral f lo hi step)
  (* step (sum (sequence-map f (in-range lo hi step)))))

(define (sum seq) (sequence-fold + 0 seq))
