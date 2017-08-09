#lang sicp

(define (accumulate op init seq)
  (letrec ((a (lambda (seq)
                (if (null? seq)
                    init
                    (op (car seq) (a (cdr seq)))))))
    (a seq)))

(define (filter pred? seq)
  (letrec ((f (lambda (seq)
                (cond ((null? seq) '())
                      ((pred? (car seq)) (cons (car seq) (f (cdr seq))))
                      (else (f (cdr seq)))))))
    (f seq)))

(define (enumerate-interval i lim)
  (letrec ((e (lambda (i)
                (if (> i lim)
                    '()
                    (cons i (e (inc i)))))))
    (e i)))

(define (flatmap p seq)
  (accumulate append '() (map p seq)))