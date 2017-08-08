#lang sicp

(define (fold-right op init seq)
  (letrec ((f (lambda (seq)
                (if (null? seq)
                    init
                    (op (car seq) (f (cdr seq)))))))
    (f seq)))

(define (fold-left op init seq)
  (letrec ((f (lambda (result rest)
                (if (null? rest)
                    result
                    (f (op result (car rest))
                       (cdr rest))))))
    (f init seq)))

(define (displayln x)
  (display x)
  (newline))

(displayln (fold-right / 1 '(1 2 3)))
;; 3/2

(displayln (fold-left / 1 '(1 2 3)))
;; 1/6

(displayln (fold-right list '() '(1 2 3)))
;; (1 (2 (3 ())))

(displayln (fold-left list '() '(1 2 3)))
;; (((() 1) 2) 3)

;; For fold-right and fold-left to yield the same result, the commutative
;; property must hold for the operator, which is to say that if the left and
;; right operands are switched, the result of an operation will be the same.

(displayln (fold-right + 0 '(1 2 3)))
(displayln (fold-left + 0 '(1 2 3)))
;; 6
;; 6

(displayln (fold-right * 1 '(1 2 3)))
(displayln (fold-left * 1 '(1 2 3)))
;; 6
;; 6
