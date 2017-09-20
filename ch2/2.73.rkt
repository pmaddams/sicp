#lang sicp

(#%require (only racket/base
                 make-hash
                 hash-has-key?
                 hash-ref
                 hash-set!))

(define (make-table)
  (let* ((table (make-hash))
         (get (lambda (k1 k2)
                (if (hash-has-key? table k1)
                    (let ((subtable (hash-ref table k1)))
                      (if (hash-has-key? subtable k2)
                          (hash-ref subtable k2)
                          #f))
                    #f)))
         (put (lambda (k1 k2 v)
                (if (not (hash-has-key? table k1))
                    (hash-set! table k1 (make-hash)))
                (let ((subtable (hash-ref table k1)))
                  (hash-set! subtable k2 v))))
         (dispatch (lambda (m)
                     (case m
                       ('get get)
                       ('put put)
                       (else (error "table: unknown method:" m))))))
    dispatch))

(define table (make-table))

(define get (table 'get))

(define put (table 'put))

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

(define (deriv-sum operands var)
  (let ((addend (car operands))
        (augend (cadr operands)))
    (make-sum (deriv addend var)
              (deriv augend var))))

(define (deriv-product operands var)
  (let ((multiplier (car operands))
        (multiplicand (cadr operands)))
    (make-sum (make-product multiplier
                            (deriv multiplicand var))
              (make-product (deriv multiplier var)
                            multiplicand))))

(define (deriv-exponentiation operands var)
  (let ((base (car operands))
        (exponent (cadr operands)))
    (make-product (make-product exponent
                                (make-exponentiation base
                                                     (make-sum exponent '-1)))
                  (deriv base var))))

(define operator car)

(define operands cdr)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var)
                             1
                             0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

;; We have defined sums, products, and exponentiations as types of arithmetic
;; expressions. We cannot place the predicates number?, variable?, and 
;; same-variable? in the table, because the it is a lookup table for operands,
;; not for special cases of derivatives.

(put 'deriv '+ deriv-sum)

(put 'deriv '* deriv-product)

(put 'deriv '** deriv-exponentiation)

(define (displayln x)
  (display x)
  (newline))

(displayln (deriv '(+ (** x 3) (* 2 x) 1) 'x))
;; (+ (* 3 (** x 2)) 2)

(displayln (deriv '(** (+ (** x 2) 1) 2) 'x))
;; (* (* 2 (+ (** x 2) 1)) (* 2 x))

(set! deriv (lambda (exp var)
              (cond ((number? exp) 0)
                    ((variable? exp) (if (same-variable? exp var)
                                         1
                                         0))
                    (else ((get (operator exp) 'deriv) (operands exp)
                                                       var)))))

(put '+ 'deriv deriv-sum)

(put '* 'deriv deriv-product)

(put '** 'deriv deriv-exponentiation)

(displayln (deriv '(+ (** x 3) (* 2 x) 1) 'x))
;; (+ (* 3 (** x 2)) 2)

(displayln (deriv '(** (+ (** x 2) 1) 2) 'x))
;; (* (* 2 (+ (** x 2) 1)) (* 2 x))

;; We are only required to install the procedures into a different subtable.
