#lang sicp

(define (displayln x)
  (begin (display x)
         (newline)))

(define x (list 1 2 3))

(define y (list 4 5 6))

(displayln (append x y))
;; (1 2 3 4 5 6)

(displayln (cons x y))
;; ((1 2 3) 4 5 6)

(displayln (list x y))
;; ((1 2 3) (4 5 6))