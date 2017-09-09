#lang sicp

(define (cons x y)
  (lambda (m)
    (m x y)))

(define (car z)
  (z (lambda (x y)
       x)))

(define (cdr z)
  (z (lambda (x y)
       y)))

(define (displayln x)
  (display x)
  (newline))

(let ((z (cons 'x 'y)))
  (displayln (car z))
  (displayln (cdr z)))
;; x
;; y
