#lang sicp

(define (for-each p l)
  (if (not (null? l))
      (begin (p (car l))
             (for-each p (cdr l)))))

(define (displayln x)
  (display x)
  (newline))

(for-each displayln '(57 321 88))
;; 57
;; 321
;; 88
