#lang racket/base

; Exercise 2.85

(require racket/function
         racket/list)

(struct number (type val))

(define table (make-hash))

(define (put k v) (hash-set! table k v))

(define (get k) (hash-ref table k))

(define (add n m) (apply-generic 'add n m))

(define (sub n m) (apply-generic 'sub n m))

(define (mul n m) (apply-generic 'mul n m))

(define (div n m) (apply-generic 'div n m))

(define (apply-generic op . args)
  (let* ((args* (coerce args))
         (t (number-type (car args*)))
         (f (get (cons op t))))
    (simplify (apply f args*))))

(define (coerce args)
  (let* ((levels (map level args))
         (top (apply max levels)))
    (for/fold ((acc '()))
              ((a (in-list args))
               (l (in-list levels)))
      (cons ((repeated super (- top l)) a) acc))))

(define (level n)
  (let ((t (number-type n)))
    (index-of '(integer rational real complex) t)))

(define (super n)
  (let* ((t (number-type n))
         (f (get (cons 'super t))))
    (f n)))

(define (simplify n)
  (let* ((t (number-type n))
         (f (get (cons 'simplify t))))
    (f n)))

(define (show n)
  (let* ((t (number-type n))
         (f (get (cons 'show t))))
    (f n)))

(define (make-integer v)
  (number 'integer v))

(define (integer? n)
  (eq? 'integer (number-type n)))

(define (install-integer-package)
  (put '(add . integer)
       (lambda (n m)
         (make-integer
          (+ (number-val n) (number-val m)))))

  (put '(sub . integer)
       (lambda (n m)
         (make-integer
          (- (number-val n) (number-val m)))))

  (put '(mul . integer)
       (lambda (n m)
         (make-integer
          (* (number-val n) (number-val m)))))

  (put '(div . integer)
       (lambda (n m)
         (make-integer
          (/ (number-val n) (number-val m)))))

  (put '(super . integer)
       (lambda (n)
         (make-rational
          (number-val n)
          1)))

  (put '(simplify . integer)
       (lambda (n)
         (make-integer
          (inexact->exact (number-val n)))))

  (put '(show . integer)
       (lambda (n)
         (number->string (number-val n)))))

(install-integer-package)

(define (make-rational n d)
  (let* ((g (gcd n d))
         (n* (/ n g))
         (d* (/ d g)))
    (if (negative? d*)
        (number 'rational (cons (- n*) (- d*)))
        (number 'rational (cons n* d*)))))

(define (rational? n)
  (eq? 'rational (number-type n)))

(define (numer n)
  (if (rational? n)
      (car (number-val n))
      (error "not a rational number")))

(define (denom n)
  (if (rational? n)
      (cdr (number-val n))
      (error "not a rational number")))

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

  (put '(super . rational)
       (lambda (n)
         (make-real
          (/ (numer n) (denom n)))))

  (put '(simplify . rational)
       (lambda (n)
         (let ((n* (make-rational (numer n) (denom n))))
           (if (= 1 (denom n*))
               (make-integer (numer n*))
               n*))))

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
  (eq? 'real (number-type n)))

(define (install-real-package)
  (put '(add . real)
       (lambda (n m)
         (make-real
          (+ (number-val n) (number-val m)))))

  (put '(sub . real)
       (lambda (n m)
         (make-real
          (- (number-val n) (number-val m)))))

  (put '(mul . real)
       (lambda (n m)
         (make-real
          (* (number-val n) (number-val m)))))

  (put '(div . real)
       (lambda (n m)
         (make-real
          (/ (number-val n) (number-val m)))))

  (put '(super . real)
       (lambda (n)
         (make-rectangular
          (number-val n)
          0.0)))

  (put '(simplify . real)
       (lambda (n)
         (let ((v (number-val n)))
           (if (= v (round v)) (make-integer v) n))))

  (put '(show . real)
       (lambda (n)
         (number->string (number-val n)))))

(install-real-package)

(define (make-rectangular r i)
  (number 'rectangular (cons r i)))

(define (rectangular? n)
  (eq? 'rectangular (number-type n)))

(define (make-polar m a)
  (number 'polar m a))

(define (polar? n)
  (eq? 'polar (number-type n)))

(define (complex? n)
  (or (rectangular? n) (polar? n)))

(define (real-part n)
  (cond ((rectangular? n) (car (number-val n)))
        ((polar? n) (* (magnitude n) (cos (angle n))))
        (else (error "not a complex number"))))

(define (imag-part n)
  (cond ((rectangular? n) (cdr (number-val n)))
        ((polar? n) (* (magnitude n) (sin (angle n))))
        (else (error "not a complex number"))))

(define (magnitude n)
  (cond ((rectangular? n) (let ((r (real-part n))
                                (i (imag-part n)))
                            (sqrt (+ (* r r) (* i i)))))
        ((polar? n) (car (number-val n)))
        (else (error "not a complex number"))))

(define (angle n)
  (cond ((rectangular? n) (atan (real-part n) (imag-part n)))
        ((polar? n) (cdr (number-val n)))
        (else (error "not a complex number"))))

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
               r
               (make-rectangular r i)))))

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
