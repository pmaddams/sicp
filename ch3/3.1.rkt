#lang sicp

(define (make-accumulator amount)
  (lambda (n)
    (set! amount (+ amount n))
    amount))

(define (displayln x)
  (display x)
  (newline))

(let ((A (make-accumulator 5)))
  (displayln (A 10))
  (displayln (A 10)))
;; 15
;; 25
