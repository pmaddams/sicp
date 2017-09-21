#lang sicp

(define op car)

(define (=zero? f)
  (and (number? f)
       (zero? f)))

(define (=one? f)
  (and (number? f)
       (= f 1)))

(define variable? symbol?)

(define (same-variable? f g)
  (and (variable? f)
       (variable? g)
       (eq? f g)))

(define (make-sum f g)
  (cond ((=zero? f)
         g)
        ((=zero? g)
         f)
        ((and (number? f)
              (number? g))
         (+ f g))
        (else
         (list '+ f g))))

(define (sum? exp)
  (and (pair? exp)
       (eq? (op exp) '+)))

(define addend cadr)

(define augend caddr)

(define (make-product f g)
  (cond ((or (=zero? f)
             (=zero? g))
         0)
        ((=one? f)
         g)
        ((=one? g)
         f)
        ((and (number? f)
              (number? g))
         (* f g))
        (else
         (list '* f g))))

(define (product? exp)
  (and (pair? exp)
       (eq? (op exp) '*)))

(define multiplier cadr)

(define multiplicand caddr)

(define (make-exponentiation f g)
  (cond ((=zero? f)
         0)
        ((=one? f)
         1)
        ((=zero? g)
         1)
        ((=one? g)
         f)
        ((and (number? f)
              (number? g))
         (expt f g))
        (else
         (list '** f g))))

(define (exponentiation? exp)
  (and (pair? exp)
       (eq? (op exp) '**)))

(define base cadr)

(define exponent caddr)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (make-sum (exponent exp)
                                                                    '-1)))
                       (deriv (base exp) var)))
        (else (error "deriv: unknown type:" exp))))

(define (displayln x)
  (display x)
  (newline))

(for-each displayln
          (list (deriv '(* x x) 'x)
                (deriv '(* (+ x 2) (+ x 3)) 'x)
                (deriv '(** (+ (* 3 x) 2) 2) 'x)))
;; (+ x x)
;; (+ (+ x 2) (+ x 3))
;; (* (* 2 (+ (* 3 x) 2)) 3)
