#lang sicp

(define (subsets s)
  (if (null? s)
      '(())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))

;; The base case occurs when we have traversed the list to the end, and returns
;; the empty set. What we want is to keep this, and attach the mapped list of
;; the preceding element consed to each subset so far.

(display (subsets '(1 2 3)))
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
