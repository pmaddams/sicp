#lang sicp

(define x (list 1 2 3))

(define y (list 4 5 6))

(begin (display (append x y))
       (newline))
;; (1 2 3 4 5 6)

(begin (display (cons x y))
       (newline))
;; ((1 2 3) 4 5 6)

(begin (display (list x y))
       (newline))
;; ((1 2 3) (4 5 6))