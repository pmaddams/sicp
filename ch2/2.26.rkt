#lang sicp

(define (displayln x)
  (display x)
  (newline))

(let ((x '(1 2 3))
      (y '(4 5 6)))
  (displayln (append x y))
  (displayln (cons x y))
  (displayln (list x y)))
;; (1 2 3 4 5 6)
;; ((1 2 3) 4 5 6)
;; ((1 2 3) (4 5 6))
