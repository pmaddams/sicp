#lang sicp

(define (foreach p l)
  (if (not (null? l))
      (begin (p (car l))
             (foreach p (cdr l)))))

(define (displayln x)
  (display x)
  (newline))

(let ((l '(57 321 88)))
  (foreach displayln l))
;; 57
;; 321
;; 88