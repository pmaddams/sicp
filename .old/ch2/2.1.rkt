#lang sicp

(define (make-rat n d)
  (let* ((g (gcd n d))
         (n (/ n g))
         (d (/ d g)))
    (if (negative? d)
        (cons (- n) (- d))
        (cons n d))))

(define numer car)

(define denom cdr)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (let ((d (denom x)))
    (if (not (= d 1))
        (begin (display "/")
               (display d))))
  (newline))

(print-rat (add-rat (make-rat -1 2)
                    (make-rat 1 -3)))
;; -5/6

(print-rat (sub-rat (make-rat -1 2)
                    (make-rat 1 -3)))
;; -1/6

(print-rat (mul-rat (make-rat -1 2)
                    (make-rat 1 -3)))
;; 1/6

(print-rat (div-rat (make-rat -1 2)
                    (make-rat 1 -3)))
;; 3/2

(equal-rat? (make-rat -1 2)
            (make-rat 1 -2))
;; #t

(equal-rat? (make-rat -1 2)
            (make-rat -2 1))
;; #f