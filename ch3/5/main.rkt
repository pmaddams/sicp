#lang racket/base

; Exercise 3.5: Monte Carlo method

(define (estimate-integral make-experiment x1 x2 y1 y2 trials)
  (* (monte-carlo trials (make-experiment x1 x2 y1 y2))
     (- x2 x1)
     (- y2 y1)))

(define (monte-carlo trials experiment)
  (let loop ((remaining trials) (passed 0))
    (cond ((zero? remaining) (/ passed trials))
          ((experiment) (loop (sub1 remaining) (add1 passed)))
          (else (loop (sub1 remaining) passed)))))

(define (estimate-pi trials)
  (estimate-integral make-circle-experiment -1.0 1.0 -1.0 1.0 trials))

(define (make-circle-experiment x1 x2 y1 y2)
  (let ((radius (/ (- x2 x1) 2)))
    (if (not (= radius (/ (- y2 y1) 2)))
        (error "invalid bounds:" x1 x2 y1 y2)
        (lambda ()
          (let ((x (random-in-range x1 x2))
                (y (random-in-range y1 y2)))
            (<= (+ (square x) (square y))
                (square radius)))))))

(define (random-in-range lo hi)
  (+ lo (* (random) (- hi lo))))

(define (square n) (* n n))
