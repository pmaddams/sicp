#lang sicp

(define (foldr proc init l)
  (letrec ((f (lambda (l)
                (if (null? l)
                    init
                    (proc (car l)
                          (f (cdr l)))))))
    (f l)))

(define (foldl proc init l)
  (letrec ((f (lambda (l result)
                (if (null? l)
                    result
                    (f (cdr l)
                       (proc (car l)
                             result))))))
    (f l init)))

(define (displayln x)
  (display x)
  (newline))

(let ((l '(1 2 3)))
  (for-each displayln
            (list (foldr / 1 l)
                  (foldl / 1 l)
                  (foldr list '() l)
                  (foldl list '() l))))
;; 3/2
;; 3/2
;; (1 (2 (3 ())))
;; (3 (2 (1 ())))

;; For foldr and foldl to yield the same result, the commutative property must
;; hold for the operator, which is to say that if the left and right operands
;; are switched, the result of an operation will be the same.
