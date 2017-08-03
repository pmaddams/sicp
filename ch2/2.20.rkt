#lang sicp

(define (same-parity first . rest)
  (letrec ((check-parity (if (odd? first)
                             odd?
                             even?))
           (build-list (lambda (rest)
                         (cond ((null? rest) '())
                               ((check-parity (car rest))
                                (cons (car rest) (build-list (cdr rest))))
                               (else (build-list (cdr rest)))))))
    (cons first (build-list rest))))