#lang racket/base

; Exercise 2.57

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((symbol? expr) (if (eq? var expr) 1 0))
        (else (case (car expr)
                ('+ (let ((u (addend expr))
                          (v (augend expr)))
                      (make-sum (deriv u var) (deriv v var))))
                ('* (let ((u (multiplier expr))
                          (v (multiplicand expr)))
                      (make-sum (make-product (deriv u var) v)
                                (make-product u (deriv v var)))))
                ('** (let ((u (base expr))
                           (n (exponent expr)))
                       (make-product (deriv u var)
                                     (make-product n (make-power u (- n 1))))))))))

(define (make-sum a1 a2)
  (cond ((eq? a1 0) a2)
        ((eq? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (eq? m1 0) (eq? m2 0)) 0)
        ((eq? m1 1) m2)
        ((eq? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-power b n)
  (cond ((eq? b 0) 0)
        ((or (eq? b 1) (zero? n)) 1)
        ((= n 1) b)
        ((number? b) (expt b n))
        (else (list '** b n))))

(define addend cadr)

(define (augend expr)
  (foldr make-sum 0 (cddr expr)))

(define multiplier cadr)

(define (multiplicand expr)
  (foldr make-product 1 (cddr expr)))

(define base cadr)

(define exponent caddr)
