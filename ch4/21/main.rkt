#lang racket/base

; Exercise 4.21: Y combinator

(provide (all-defined-out))

(define (yc f)
  (let ((r (lambda (r*)
             (lambda (x)
               ((f (r* r*)) x)))))
    (r r)))
