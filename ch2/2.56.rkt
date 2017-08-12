#lang sicp

(define variable? symbol?)

(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1)
              (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1)
              (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b n)
  (cond ((=number? b 0) 0)
        ((=number? b 1) 1)
        ((=number? n 0) 1)
        ((=number? n 1) b)
        ((and (number? b)
              (number? n)) (expt b n))
        (else (list '** b n))))

(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

(define addend cadr)

(define augend caddr)

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define multiplier cadr)

(define multiplicand caddr)

(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

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
                                                          (make-sum (exponent exp) '-1)))
                       (deriv (base exp) var)))
        (else (error "deriv: unknown expression type" exp))))

(define (displayln x)
  (display x)
  (newline))

(displayln (deriv '(+ (** x 3) (* 2 x) 1) 'x))
;; (+ (* 3 (** x 2)) 2)

(displayln (deriv '(** (+ (** x 2) 1) 2) 'x))
;; (* (* 2 (+ (** x 2) 1)) (* 2 x))