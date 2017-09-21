#lang sicp

(#%require (only racket/base
                 make-hash
                 hash-has-key?
                 hash-ref
                 hash-set!))

(define (make-table)
  (let* ((table (make-hash))
         (get (lambda (k1 k2)
                (hash-ref table
                          (list k1 k2)
                          #f)))
         (put (lambda (k1 k2 v)
                (hash-set! table
                           (list k1 k2)
                           v)))
         (dispatch (lambda (m)
                     (case m
                       ('get get)
                       ('put put)
                       (else (error "table: unknown method:" m))))))
    dispatch))

(define table (make-table))

(define get (table 'get))

(define put (table 'put))

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

(define (deriv-sum operands var)
  (let ((f (car operands))
        (g (cadr operands)))
    (make-sum (deriv f var)
              (deriv g var))))

(put 'deriv '+ deriv-sum)

(define (deriv-product operands var)
  (let ((f (car operands))
        (g (cadr operands)))
    (make-sum (make-product f
                            (deriv g var))
              (make-product (deriv f var)
                            g))))

(put 'deriv '* deriv-product)

(define (deriv-exponentiation operands var)
  (let ((f (car operands))
        (g (cadr operands)))
    (make-product (make-product g
                                (make-exponentiation f
                                                     (make-sum g '-1)))
                  (deriv f var))))

(put 'deriv '** deriv-exponentiation)

(define operator car)

(define operands cdr)

(define (deriv exp var)
  (cond ((number? exp)
         0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        (else
         ((get 'deriv (operator exp))
          (operands exp)
          var))))

;; We have defined sums, products, and exponentiations as types of arithmetic
;; expressions. We cannot place predicates such as variable? and same-variable?
;; in the table, because they operate on all types.

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

(put '+ 'deriv deriv-sum)

(put '* 'deriv deriv-product)

(put '** 'deriv deriv-exponentiation)

(set! deriv (lambda (exp var)
              (cond ((number? exp)
                     0)
                    ((variable? exp)
                     (if (same-variable? exp var)
                         1
                         0))
                    (else
                     ((get (operator exp) 'deriv)
                      (operands exp)
                      var)))))

(for-each displayln
          (list (deriv '(* x x) 'x)
                (deriv '(* (+ x 2) (+ x 3)) 'x)
                (deriv '(** (+ (* 3 x) 2) 2) 'x)))
;; (+ x x)
;; (+ (+ x 2) (+ x 3))
;; (* (* 2 (+ (* 3 x) 2)) 3)

;; To adapt to a different scheme for looking up procedures, we need to
;; reinstall the procedures into the table under a different set of keys.
