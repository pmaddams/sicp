#lang sicp

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(let ((l '(23 72 149 34)))
  (display (last-pair l)))
;; (34)
