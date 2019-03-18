#lang racket/base

; Exercise 2.85

(provide (all-defined-out))

(require racket/function
         racket/list)

(struct number (type val) #:transparent)

(define (type n)
  (if (complex? n)
      'complex
      (number-type n)))

(define (value n)
  (number-val n))

(define (level n)
  (index-of '(integer rational real complex) (type n)))

(define table (make-hash))

(define (get k1 k2)
  (let ((k (cons k1 k2)))
    (hash-ref table k)))

(define (put k1 k2 v)
  (let ((k (cons k1 k2)))
    (if (hash-has-key? table k)
        (error "key exists:" k)
        (hash-set! table k v))))

(define (add n m) (apply-generic 'add n m))

(define (sub n m) (apply-generic 'sub n m))

(define (mul n m) (apply-generic 'mul n m))

(define (div n m) (apply-generic 'div n m))

(define (apply-generic op . args)
  (let* ((l (coerce args))
         (f (get op (type (car l)))))
    (simplify (apply f l))))

(define (coerce args)
  (let* ((top (apply max (map level args)))
         (f (lambda (n) ((repeated super (- top (level n))) n))))
    (map f args)))

(define (super n)
  ((get 'super (type n)) n))

(define (simplify n)
  ((get 'simplify (type n)) n))

(define (make-integer val)
  (number 'integer val))

(define (integer? n)
  (eq? 'integer (type n)))

(define (install-integer-package)
  (put 'add 'integer
       (lambda (n m)
         (make-integer
          (+ (value n) (value m)))))

  (put 'sub 'integer
       (lambda (n m)
         (make-integer
          (- (value n) (value m)))))

  (put 'mul 'integer
       (lambda (n m)
         (make-integer
          (* (value n) (value m)))))

  (put 'div 'integer
       (lambda (n m)
         (make-rational
          (value n)
          (value m))))

  (put 'super 'integer
       (lambda (n)
         (make-rational
          (value n)
          1))))

(install-integer-package)

(define (make-rational nv dv)
  (number 'rational (cons nv dv)))

(define (rational? n)
  (eq? 'rational (type n)))

(define (numer n)
  (if (rational? n)
      (car (value n))
      (error "type error")))

(define (denom n)
  (if (rational? n)
      (cdr (value n))
      (error "type error")))

(define (install-rational-package)
  (put 'add 'rational
       (lambda (n m)
         (make-rational
          (+ (* (numer n) (denom m))
             (* (denom n) (numer m)))
          (* (denom n) (denom m)))))

  (put 'sub 'rational
       (lambda (n m)
         (make-rational
          (- (* (numer n) (denom m))
             (* (denom n) (numer m)))
          (* (denom n) (denom m)))))

  (put 'mul 'rational
       (lambda (n m)
         (make-rational
          (* (numer n) (numer m))
          (* (denom n) (denom m)))))

  (put 'div 'rational
       (lambda (n m)
         (make-rational
          (* (numer n) (denom m))
          (* (denom n) (numer m)))))

  (put 'super 'rational
       (lambda (n)
         (make-real
          (/ (numer n) (denom n)))))

  (put 'simplify 'rational
       (lambda (n)
         (let* ((g (gcd (numer n) (denom n)))
                (nv (quotient (numer n) g))
                (dv (quotient (denom n) g)))
           (if (= dv 1)
               (make-integer nv)
               n)))))

(install-rational-package)

(define (make-real val)
  (number 'real val))

(define (real? n)
  (eq? 'real (type n)))

(define (install-real-package)
  (put 'add 'real
       (lambda (n m)
         (make-real
          (+ (value n) (value m)))))

  (put 'sub 'real
       (lambda (n m)
         (make-real
          (- (value n) (value m)))))

  (put 'mul 'real
       (lambda (n m)
         (make-real
          (* (value n) (value m)))))

  (put 'div 'real
       (lambda (n m)
         (make-real
          (/ (value n) (value m)))))

  (put 'super 'real
       (lambda (n)
         (make-rectangular
          (value n)
          0.0)))

  (put 'simplify 'real
       (lambda (n)
         (let ((val (value n)))
           (if (= val (round val))
               (make-integer val)
               n)))))

(install-real-package)

(define (make-rectangular rv iv)
  (number 'rectangular (cons rv iv)))

(define (rectangular? n)
  (eq? 'rectangular (number-type n)))

(define (make-polar mv av)
  (number 'polar (cons mv av)))

(define (polar? n)
  (eq? 'polar (number-type n)))

(define (complex? n)
  (or (rectangular? n) (polar? n)))

(define (real-part n)
  (cond ((rectangular? n) (car (value n)))
        ((polar? n) (* (magnitude n) (cos (angle n))))
        (else (error "type error"))))

(define (imag-part n)
  (cond ((rectangular? n) (cdr (value n)))
        ((polar? n) (* (magnitude n) (sin (angle n))))
        (else (error "type error"))))

(define (magnitude n)
  (cond ((rectangular? n) (let ((rv (real-part n))
                                (iv (imag-part n)))
                            (sqrt (+ (* rv rv) (* iv iv)))))
        ((polar? n) (car (value n)))
        (else (error "type error"))))

(define (angle n)
  (cond ((rectangular? n) (atan (real-part n) (imag-part n)))
        ((polar? n) (cdr (value n)))
        (else (error "type error"))))

(define (install-complex-package)
  (put 'add 'complex
       (lambda (n m)
         (make-rectangular
          (+ (real-part n) (real-part m))
          (+ (imag-part n) (imag-part m)))))

  (put 'sub 'complex
       (lambda (n m)
         (make-rectangular
          (- (real-part n) (real-part m))
          (- (imag-part n) (imag-part m)))))

  (put 'mul 'complex
       (lambda (n m)
         (make-polar
          (* (magnitude n) (magnitude m))
          (+ (angle n) (angle m)))))

  (put 'div 'complex
       (lambda (n m)
         (make-polar
          (/ (magnitude n) (magnitude m))
          (- (angle n) (angle m)))))

  (put 'simplify 'complex
       (lambda (n)
         (let ((rv (real-part n))
               (iv (imag-part n)))
           (if (zero? iv)
               (simplify (make-real rv))
               n)))))

(install-complex-package)

(define (repeated f n)
  (for/fold ((acc identity))
            ((i (in-range n)))
    (compose f acc)))
