#lang racket/base

; Exercise 2.57: Symbolic differentiation

(require racket/match)

(define (deriv expr var)
  (match expr
    ((? number?) 0)
    ((? symbol? expr) (if (eq? expr var) 1 0))
    ((list '+ u v) `(+ ,(deriv u var) ,(deriv v var)))
    ((list '* u v) `(+ (* ,(deriv u var) ,v) (* ,u ,(deriv v var))))
    ((list '** u n) `(* ,n (** ,u ,(sub1 n))))))
