#lang sicp

(define (make-accumulator sum)
  (lambda (n)
    (set! sum
          (+ sum n))
    sum))

(define (displayln x)
  (display x)
  (newline))

(let ((A (make-accumulator 5)))
  (displayln (A 10))
  (displayln (A 10)))
;; 15
;; 25
