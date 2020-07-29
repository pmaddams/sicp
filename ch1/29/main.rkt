#lang racket/base

; Exercise 1.29

(provide (all-defined-out))

(require racket/sequence)

(define (simpson-integral f lo hi step)
  (* (/ step 3)
     (let loop ((acc (f lo)) (i 1) (x (+ lo step)))
       (if (>= x hi)
           (+ acc (f x))
           (let* ((coeff (if (even? i) 2 4))
                  (term (* coeff (f x))))
             (loop (+ acc term) (add1 i) (+ x step)))))))

(define (riemann-integral f lo hi step)
  (* step (sum (sequence-map f (in-range lo hi step)))))

(define (sum seq) (sequence-fold + 0 seq))
