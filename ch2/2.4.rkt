#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(let ((p (cons 'a 'b)))
  (begin (display (car p))
         (newline)
         (display (cdr p))
         (newline)))
;; a
;; b