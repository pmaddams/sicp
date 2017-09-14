#lang sicp

(define (foldr proc init seq)
  (letrec ((f (lambda (seq)
                (if (null? seq)
                    init
                    (proc (car seq)
                          (f (cdr seq)))))))
    (f seq)))

(define (foldl proc init seq)
  (letrec ((f (lambda (seq result)
                (if (null? seq)
                    result
                    (f (cdr seq)
                       (proc (car seq)
                             result))))))
    (f seq init)))

(define (displayln x)
  (display x)
  (newline))

(let ((l '(1 2 3)))
  (displayln (foldr / 1 l))
  (displayln (foldl / 1 l))
  (displayln (foldr list '() l))
  (displayln (foldl list '() l)))
;; 3/2
;; 3/2
;; (1 (2 (3 ())))
;; (3 (2 (1 ())))

;; For foldr and foldl to yield the same result, the commutative property must
;; hold for the operator, which is to say that if the left and right operands
;; are switched, the result of an operation will be the same.
