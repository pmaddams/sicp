#lang sicp

(define (square x)
  (expt x 2))

(define (square-list-a items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-a (cdr items)))))

(define (square-list-b items)
  (map square items))