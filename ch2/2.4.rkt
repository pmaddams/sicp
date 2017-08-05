#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define (displayln x)
  (display x)
  (newline))

(let ((p (cons 'a 'b)))
  (displayln (car p))
  (displayln (cdr p)))
;; a
;; b