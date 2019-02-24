#lang racket/base

; Exercise 3.5

(provide (all-defined-out))

(require racket/function)

(define (pi-approx trials)
  (integral-approx make-circle-experiment -1.0 1.0 -1.0 1.0 trials))

(define (integral-approx make-experiment x1 x2 y1 y2 trials)
  (let ((fraction (monte-carlo trials (make-experiment x1 x2 y1 y2)))
        (area (* (- x2 x1) (- y2 y1))))
    (* fraction area)))

(define (make-circle-experiment x1 x2 y1 y2)
  (let ((r (/ (- x2 x1) 2)))
    (if (not (= r (/ (- y2 y1) 2)))
        (error "invalid bounds:" x1 x2 y1 y2)
        (thunk (let ((x (random-in-range x1 x2))
                     (y (random-in-range y1 y2)))
                 (within x y r))))))

(define (within x y r)
  (<= (+ (square x) (square y))
      (square r)))

(define (monte-carlo trials experiment)
  (/ (for/sum ((i (in-range trials)))
       (if (experiment) 1 0))
     trials))

(define (random-in-range lo hi)
  (+ lo (* (random) (- hi lo))))

(define (square n) (* n n))
