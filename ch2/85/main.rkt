#lang racket/base

; Exercise 2.85: Generic arithmetic

(require racket/function
         racket/list)

(define tab (make-hash))

(define (put k v)
  (hash-set! tab k v))

(define (get k)
  (hash-ref tab k))

(define (add n m)
  (apply-generic 'add n m))

(define (sub n m)
  (apply-generic 'sub n m))

(define (mul n m)
  (apply-generic 'mul n m))

(define (div n m)
  (apply-generic 'div n m))

(define (apply-generic op . args)
  (let ((args (coerce args)))
    (simpl (apply (get (list op (type-tag (car args)))) args))))

(define (coerce args)
  (let* ((levels (map type-level args))
         (top (apply max levels)))
    (let loop ((args args) (levels levels) (acc '()))
      (if (null? args)
          acc
          (loop (cdr args)
                (cdr levels)
                (cons ((repeated super (- top (car levels))) (car args))
                      acc))))))

(define (type-tag n)
  (car (type n)))

(define (type-level n)
  (cdr (type n)))

(define (type n)
  (cond ((int? n) '(int . 0))
        ((rat? n) '(rat . 1))
        ((re? n) '(re . 2))
        ((cmplx? n) '(cmplx . 3))
        (else (error "unknown type:" n))))

(define (repeated f n)
  (let loop ((n n) (acc identity))
    (if (zero? n)
        acc
        (loop (sub1 n) (compose f acc)))))

(define (super n)
  ((get `(super ,(type-tag n))) n))

(define (simpl n)
  ((get `(simpl ,(type-tag n))) n))

(define (show n)
  ((get `(show ,(type-tag n))) n))

(struct int (val))

(define (install-int-package)
  (put '(add int)
       (lambda (n m)
         (int (+ (int-val n) (int-val m)))))

  (put '(sub int)
       (lambda (n m)
         (int (- (int-val n) (int-val m)))))

  (put '(mul int)
       (lambda (n m)
         (int (* (int-val n) (int-val m)))))

  (put '(div int)
       (lambda (n m)
         (int (/ (int-val n) (int-val m)))))

  (put '(super int)
       (lambda (n)
         (rat (int-val n) 1)))

  (put '(simpl int)
       (lambda (n)
         (int (inexact->exact (int-val n)))))

  (put '(show int)
       (lambda (n)
         (number->string (int-val n)))))

(install-int-package)

(struct rat (numer denom))

(define (make-rat numer denom)
  (let* ((g (gcd numer denom))
         (numer (/ numer g))
         (denom (/ denom g)))
    (if (negative? denom)
        (rat (- numer) (- denom))
        (rat numer denom))))

(define (install-rat-package)
  (put '(add rat)
       (lambda (n m)
         (make-rat (+ (* (rat-numer n) (rat-denom m))
                      (* (rat-denom n) (rat-numer m)))
                   (* (rat-denom n) (rat-denom m)))))

  (put '(sub rat)
       (lambda (n m)
         (make-rat (- (* (rat-numer n) (rat-denom m))
                      (* (rat-denom n) (rat-numer m)))
                   (* (rat-denom n) (rat-denom m)))))

  (put '(mul rat)
       (lambda (n m)
         (make-rat (* (rat-numer n) (rat-numer m))
                   (* (rat-denom n) (rat-denom m)))))

  (put '(div rat)
       (lambda (n m)
         (make-rat (* (rat-numer n) (rat-denom m))
                   (* (rat-denom n) (rat-numer m)))))

  (put '(super rat)
       (lambda (n)
         (let ((int->re (lambda (v) (+ v 0.0))))
           (/ (int->re (rat-numer n))
              (int->re (rat-denom n))))))

  (put '(simpl rat)
       (lambda (n)
         (let ((n (make-rat (rat-numer n) (rat-denom n))))
           (if (= (rat-denom n) 1)
               (rat-numer n)
               n))))

  (put '(show rat)
       (lambda (n)
         (string-append
          (number->string (rat-numer n))
          "/"
          (number->string (rat-denom n))))))

(install-rat-package)

(struct re (val))

(define (install-re-package)
  (put '(add re)
       (lambda (n m)
         (re (+ (re-val n) (re-val m)))))

  (put '(sub re)
       (lambda (n m)
         (re (- (re-val n) (re-val m)))))

  (put '(mul re)
       (lambda (n m)
         (re (* (re-val n) (re-val m)))))

  (put '(div re)
       (lambda (n m)
         (re (/ (re-val n) (re-val m)))))

  (put '(super re)
       (lambda (n)
         (cmplx (re-val n) 0.0)))

  (put '(simpl re)
       (lambda (n)
         (let ((v (re-val n)))
           (if (= (round v) v) (int v) n))))

  (put '(show re)
       (lambda (n)
         (number->string (re-val n)))))

(install-re-package)

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
      (let ((square (lambda (v) (* v v))))
        (sqrt (+ (square (rect-re n))
                 (square (rect-im n)))))))

(define (cmplx-ang n)
  (if (polar? n)
      (polar-mag n)
      (atan (rect-im n)
            (rect-re n))))

(define (install-cmplx-package)
  (put '(add cmplx)
       (lambda (n m)
         (rect (+ (cmplx-re n) (cmplx-re m))
               (+ (cmplx-im n) (cmplx-im m)))))

  (put '(sub cmplx)
       (lambda (n m)
         (rect (- (cmplx-re n) (cmplx-re m))
               (- (cmplx-im n) (cmplx-im m)))))

  (put '(mul cmplx)
       (lambda (n m)
         (polar (* (cmplx-mag n) (cmplx-mag m))
                (+ (cmplx-ang n) (cmplx-ang m)))))

  (put '(div cmplx)
       (lambda (n m)
         (polar (/ (cmplx-mag n) (cmplx-mag m))
                (- (cmplx-ang n) (cmplx-ang m)))))

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

(install-cmplx-package)
