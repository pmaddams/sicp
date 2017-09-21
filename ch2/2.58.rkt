#lang sicp

(define (op exp)
  (cond ((memq '+ exp) '+)
        ((memq '* exp) '*)
        ((memq '** exp) '**)
        (else (error "op: invalid syntax:" exp))))

(define (before item exp)
  (letrec ((b (lambda (exp result)
                (cond ((null? exp)
                       (error "before: invalid syntax:" exp))
                      ((eq? item (car exp))
                       result)
                      (else (b (cdr exp)
                               (append result (list (car exp)))))))))
    (let ((r (b exp '())))
      (if (null? (cdr r))
          (car r)
          r))))

(define (after item exp)
  (let* ((m (memq item exp))
         (r (if m
                (cdr m)
                (error "after: invalid syntax:" exp))))
    (if (null? (cdr r))
        (car r)
        r)))

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
         (list f '+ g))))

(define (sum? x)
  (eq? (op x) '+))

(define (addend s)
  (before '+ s))

(define (augend s)
  (after '+ s))

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
         (list f '* g))))

(define (product? x)
  (eq? (op x) '*))

(define (multiplier p)
  (before '* p))

(define (multiplicand p)
  (after '* p))

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
         (list f '** g))))

(define (exponentiation? x)
  (eq? (op x) '**))

(define (base e)
  (before '** e))

(define (exponent e)
  (after '** e))

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
          (list (deriv '(x ** 3 + 2 * x + 1) 'x)
                (deriv '((x ** 2 + 1) ** 2) 'x)
                (deriv '(x + x + 1 + 1) 'x)))
;; ((3 * (x ** 2)) + 2)
;; ((2 * (x ** 2 + 1)) * (2 * x))
;; 2
