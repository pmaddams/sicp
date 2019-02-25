#lang racket/base

; Exercise 4.21

(provide (all-defined-out))

(define (Y f)
  (let ((r (lambda (r*)
             (lambda (x)
               ((f (r* r*)) x)))))
    (r r)))
