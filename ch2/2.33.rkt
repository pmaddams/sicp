#lang sicp

(define (square x)
  (expt x 2))

(define (fib n)
  (letrec ((f (lambda (a b p q count)
                (cond ((zero? count) b)
                      ((even? count) (f a
                                        b
                                        (+ (expt p 2)
                                           (expt q 2))
                                        (+ (* 2 p q)
                                           (expt q 2))
                                        (/ count 2)))
                      (else (f (+ (* b q) (* a q) (* a p))
                               (+ (* b p) (* a q))
                               p
                               q
                               (dec count)))))))
    (f 1 0 0 1 n)))

(define (filter pred? seq)
  (letrec ((f (lambda (seq)
                (cond ((null? seq) '())
                      ((pred? (car seq)) (cons (car seq) (f (cdr seq))))
                      (else (f (cdr seq)))))))
    (f seq)))

(define (accumulate op init seq)
  (letrec ((a (lambda (seq)
                (if (null? seq)
                    init
                    (op (car seq) (a (cdr seq)))))))
    (a seq)))