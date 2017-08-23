#lang sicp

(define (make-table same-key?)
  (let* ((table (list '<table>))
         (assoc (lambda (key records)
                  (letrec ((a (lambda (records)
                                (cond ((null? records) #f)
                                      ((same-key? key (caar records)) (car records))
                                      (else (a (cdr records)))))))
                    (a records))))
         (lookup (lambda (key1 key2)
                   (let ((subtable (assoc key1 (cdr table))))
                     (if subtable
                         (let ((record (assoc key2 (cdr subtable))))
                           (if record
                               (cdr record)
                               #f))
                         #f))))
         (insert! (lambda (key1 key2 value)
                    (let ((subtable (assoc key1 (cdr table))))
                      (if subtable
                          (let ((record (assoc key2 (cdr subtable))))
                            (if record
                                (set-cdr! record value)
                                (set-cdr! subtable
                                          (cons (cons key2 value)
                                                (cdr subtable)))))
                          (set-cdr! table
                                    (cons (list key1
                                                (cons key2 value))
                                          (cdr table)))))))
         (dispatch (lambda (m)
                     (cond ((eq? m 'lookup) lookup)
                           ((eq? m 'insert!) insert!)
                           (else (error "make-table: undefined operation:" m))))))
    dispatch))

(define table (make-table equal?))

(define get (table 'lookup))

(define put (table 'insert!))

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
