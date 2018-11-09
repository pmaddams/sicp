#lang racket/base

; Exercise 4.21: Y combinator

(define (yc f)
  (let ((r (lambda (r*)
             (lambda (x)
               ((f (r* r*)) x)))))
    (r r)))
