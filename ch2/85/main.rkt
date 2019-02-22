#lang racket/base

; Exercise 2.85

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

(define (put k v) (hash-set! table k v))

(define (get k) (hash-ref table k))

(define (add n m) (apply-generic 'add n m))

(define (sub n m) (apply-generic 'sub n m))

(define (mul n m) (apply-generic 'mul n m))

(define (div n m) (apply-generic 'div n m))

(define (apply-generic op . args)
  (let* ((args* (coerce args))
         (f (get (cons op (type (car args*))))))
    (simplify (apply f args*))))

(define (coerce args)
  (let* ((top (apply max (map level args)))
         (f (lambda (n) ((repeated raise (- top (level n))) n))))
    (map f args)))

(define (raise n)
  ((get `(raise . ,(type n))) n))

(define (simplify n)
  ((get `(simplify . ,(type n))) n))

(define (show n)
  ((get `(show . ,(type n))) n))

(define (make-integer v)
  (number 'integer v))

(define (integer? n)
  (eq? 'integer (type n)))

(define (install-integer-package)
  (put '(add . integer)
       (lambda (n m)
         (make-integer
          (+ (value n) (value m)))))

  (put '(sub . integer)
       (lambda (n m)
         (make-integer
          (- (value n) (value m)))))

  (put '(mul . integer)
       (lambda (n m)
         (make-integer
          (* (value n) (value m)))))

  (put '(div . integer)
       (lambda (n m)
         (make-rational
          (value n)
          (value m))))

  (put '(raise . integer)
       (lambda (n)
         (make-rational
          (value n)
          1)))

  (put '(simplify . integer)
       (lambda (n)
         (make-integer
          (inexact->exact (value n)))))

  (put '(show . integer)
       (lambda (n)
         (number->string (value n)))))

(install-integer-package)

(define (make-rational n d)
  (number 'rational (cons n d)))

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
  (put '(add . rational)
       (lambda (n m)
         (make-rational
          (+ (* (numer n) (denom m))
             (* (denom n) (numer m)))
          (* (denom n) (denom m)))))

  (put '(sub . rational)
       (lambda (n m)
         (make-rational
          (- (* (numer n) (denom m))
             (* (denom n) (numer m)))
          (* (denom n) (denom m)))))

  (put '(mul . rational)
       (lambda (n m)
         (make-rational
          (* (numer n) (numer m))
          (* (denom n) (denom m)))))

  (put '(div . rational)
       (lambda (n m)
         (make-rational
          (* (numer n) (denom m))
          (* (denom n) (numer m)))))

  (put '(raise . rational)
       (lambda (n)
         (make-real
          (/ (numer n) (denom n)))))

  (put '(simplify . rational)
       (lambda (n)
         (let* ((g (gcd (numer n) (denom n)))
                (n* (quotient (numer n) g))
                (d* (quotient (denom n) g)))
           (if (= d* 1)
               (make-integer n*)
               n))))

  (put '(show . rational)
       (lambda (n)
         (string-append
          (number->string (numer n))
          "/"
          (number->string (denom n))))))

(install-rational-package)

(define (make-real v)
  (number 'real v))

(define (real? n)
  (eq? 'real (type n)))

(define (install-real-package)
  (put '(add . real)
       (lambda (n m)
         (make-real
          (+ (value n) (value m)))))

  (put '(sub . real)
       (lambda (n m)
         (make-real
          (- (value n) (value m)))))

  (put '(mul . real)
       (lambda (n m)
         (make-real
          (* (value n) (value m)))))

  (put '(div . real)
       (lambda (n m)
         (make-real
          (/ (value n) (value m)))))

  (put '(raise . real)
       (lambda (n)
         (make-rectangular
          (value n)
          0.0)))

  (put '(simplify . real)
       (lambda (n)
         (let ((v (value n)))
           (if (= v (round v))
               (make-integer v)
               n))))

  (put '(show . real)
       (lambda (n)
         (number->string (value n)))))

(install-real-package)

(define (make-rectangular r i)
  (number 'rectangular (cons r i)))

(define (rectangular? n)
  (eq? 'rectangular (number-type n)))

(define (make-polar m a)
  (number 'polar (cons m a)))

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
  (cond ((rectangular? n) (let ((r (real-part n))
                                (i (imag-part n)))
                            (sqrt (+ (* r r) (* i i)))))
        ((polar? n) (car (value n)))
        (else (error "type error"))))

(define (angle n)
  (cond ((rectangular? n) (atan (real-part n) (imag-part n)))
        ((polar? n) (cdr (value n)))
        (else (error "type error"))))

(define (install-complex-package)
  (put '(add . complex)
       (lambda (n m)
         (make-rectangular
          (+ (real-part n) (real-part m))
          (+ (imag-part n) (imag-part m)))))

  (put '(sub . complex)
       (lambda (n m)
         (make-rectangular
          (- (real-part n) (real-part m))
          (- (imag-part n) (imag-part m)))))

  (put '(mul . complex)
       (lambda (n m)
         (make-polar
          (* (magnitude n) (magnitude m))
          (+ (angle n) (angle m)))))

  (put '(div . complex)
       (lambda (n m)
         (make-polar
          (/ (magnitude n) (magnitude m))
          (- (angle n) (angle m)))))

  (put '(simplify . complex)
       (lambda (n)
         (let ((r (real-part n))
               (i (imag-part n)))
           (if (zero? i)
               (simplify (make-real r))
               n))))

  (put '(show . complex)
       (lambda (n)
         (string-append
          (number->string (real-part n))
          "+"
          (number->string (imag-part n))
          "i"))))

(install-complex-package)

(define (repeated f n)
  (for/fold ((acc identity))
            ((i (in-range n)))
    (compose f acc)))
