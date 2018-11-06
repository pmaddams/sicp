#lang racket/base

; Exercise 2.85: Generic arithmetic

(require racket/function
         racket/list)

(define (type n)
  (cond ((int? n) 'int)
        ((rat? n) 'rat)
        ((re? n) 're)
        ((cmplx? n) 'cmplx)
        (else (error "unknown type" n))))

(define (level n)
  (let ((tower '(int rat re cmplx)))
    (index-of tower (type n))))

(define table (make-hash))

(define (get k)
  (hash-ref table k))

(define (put k v)
  (hash-set! table k v))

(define (super n)
  ((get `(super ,(type n))) n))

(define (simpl n)
  ((get `(simpl ,(type n))) n))

(define (show n)
  ((get `(show ,(type n))) n))

(define (repeated n f)
  (if (zero? n)
      identity
      (compose f (repeated (sub1 n) f))))

(define (coerce args)
  (let* ((levels (map level args))
         (top (apply max levels)))
    (letrec
        ((c (lambda (args levels)
              (if (null? args)
                  '()
                  (cons ((repeated (- top (car levels)) super) (car args))
                        (c (cdr args) (cdr levels)))))))
      (c args levels))))

(define (apply-generic op args)
  (let* ((args (coerce args))
         (k (list op (type (car args))))
         (v (get k)))
    (simpl (apply v args))))

(define (add . args)
  (apply-generic 'add args))

(define (sub . args)
  (apply-generic 'sub args))

(define (mul . args)
  (apply-generic 'mul args))

(define (div . args)
  (apply-generic 'div args))

(define (flip f)
  (lambda (x y)
    (f y x)))

(define (fold op args)
  (foldl (flip op) (car args) (cdr args)))

(define int? integer?)

(define (install-int)
  (put '(add int) +)

  (put '(sub int) -)

  (put '(mul int) *)

  (put '(div int)
       (lambda args
         (apply div (map (lambda (n) (make-rat n 1)) args))))

  (put '(super int)
       (lambda (n) (rat n 1)))

  (put '(simpl int) inexact->exact)

  (put '(show int) number->string))

(install-int)

(struct rat (numer denom))

(define (make-rat numer denom)
  (let* ((g (gcd numer denom))
         (numer (/ numer g))
         (denom (/ denom g)))
    (if (negative? denom)
        (rat (- numer) (- denom))
        (rat numer denom))))

(define (install-rat)
  (define (add n1 n2)
    (make-rat (+ (* (rat-numer n1) (rat-denom n2))
                 (* (rat-denom n1) (rat-numer n2)))
              (* (rat-denom n1) (rat-denom n2))))

  (put '(add rat) (lambda args (fold add args)))

  (define (sub n1 n2)
    (make-rat (- (* (rat-numer n1) (rat-denom n2))
                 (* (rat-denom n1) (rat-numer n2)))
              (* (rat-denom n1) (rat-denom n2))))

  (put '(sub rat)
       (lambda args
         (if (= (length args) 1)
             (sub (rat 0 0) (car args))
             (fold sub args))))

  (define (mul n1 n2)
    (make-rat (* (rat-numer n1) (rat-numer n2))
              (* (rat-denom n1) (rat-denom n2))))

  (put '(mul rat) (lambda args (fold mul args)))

  (define (div n1 n2)
    (make-rat (* (rat-numer n1) (rat-denom n2))
              (* (rat-denom n1) (rat-numer n2))))

  (put '(div rat) (lambda args (fold div args)))

  (put '(super rat)
       (lambda (n)
         (let ((int->re (lambda (i) (+ i 0.0))))
           (/ (int->re (rat-numer n))
              (int->re (rat-denom n))))))

  (put '(simpl rat)
       (lambda (n)
         (let ((n (make-rat (rat-numer n) (rat-denom n))))
           (if (= 1 (rat-denom n))
               (rat-numer n)
               n))))

  (put '(show rat)
       (lambda (n)
         (string-append
          (number->string (rat-numer n))
          "/"
          (number->string (rat-denom n))))))

(install-rat)

(define re? real?)

(define (install-re)
  (put '(add re) +)

  (put '(sub re) -)

  (put '(mul re) *)

  (put '(div re) /)

  (put '(super re)
       (lambda (n)
         (cmplx n 0.0)))

  (put '(simpl re)
       (lambda (n)
         (let ((epsilon 0.0000000001)
               (i (round n)))
           (if (< (abs (- i n)) epsilon) i n))))

  (put '(show re) number->string))

(install-re)

(struct rect (re im))

(struct polar (mag ang))

(define (cmplx re im)
  (rect re im))

(define (cmplx? n)
  (or (rect? n) (polar? n)))

(define (cmplx-re n)
  (if (rect? n)
      (rect-re n)
      (* (polar-mag n) (cos (polar-ang n)))))

(define (cmplx-im n)
  (if (rect? n)
      (rect-im n)
      (* (polar-mag n) (sin (polar-ang n)))))

(define (cmplx-mag n)
  (if (polar? n)
      (polar-mag n)
      (let ((square (lambda (x) (* x x))))
        (sqrt (+ (square (rect-re n))
                 (square (rect-im n)))))))

(define (cmplx-ang n)
  (if (polar? n)
      (polar-mag n)
      (atan (rect-im n)
            (rect-re n))))

(define (install-cmplx)
  (define (add n1 n2)
    (rect (+ (cmplx-re n1) (cmplx-re n2))
          (+ (cmplx-im n1) (cmplx-im n2))))

  (put '(add cmplx) (lambda args (fold add args)))

  (define (sub n1 n2)
    (rect (- (cmplx-re n1) (cmplx-re n2))
          (- (cmplx-im n1) (cmplx-im n2))))

  (put '(sub cmplx)
       (lambda args
         (if (= (length args) 1)
             (sub (cmplx 0.0 0.0) (car args))
             (fold sub args))))

  (define (mul n1 n2)
    (polar (* (cmplx-mag n1) (cmplx-mag n2))
           (+ (cmplx-ang n1) (cmplx-ang n2))))

  (put '(mul cmplx) (lambda args (fold mul args)))

  (define (div n1 n2)
    (polar (/ (cmplx-mag n1) (cmplx-mag n2))
           (- (cmplx-ang n1) (cmplx-ang n2))))

  (put '(div cmplx) (lambda args (fold div args)))

  (put '(simpl cmplx)
       (lambda (n)
         (let ((re (simpl (cmplx-re n)))
               (im (simpl (cmplx-im n))))
           (if (zero? im)
               re
               (cmplx re im)))))

  (put '(show cmplx)
       (lambda (n)
         (string-append
          (number->string (cmplx-re n))
          "+"
          (number->string (cmplx-im n))
          "i"))))

(install-cmplx)
