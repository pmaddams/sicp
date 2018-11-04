#lang racket/base

; Exercise 2.12: Interval arithmetic

(struct interval (lower upper) #:transparent)

(define (interval-add i1 i2)
  (interval (+ (interval-lower i1)
               (interval-lower i2))
            (+ (interval-upper i1)
               (interval-upper i2))))

(define (interval-sub i1 i2)
  (interval (- (interval-lower i1)
               (interval-lower i2))
            (- (interval-upper i1)
               (interval-upper i2))))

(define (interval-mul i1 i2)
  (let ((l1 (interval-lower i1))
        (u1 (interval-upper i1))
        (l2 (interval-lower i2))
        (u2 (interval-upper i2)))
    (cond ((not (negative? l1)) (cond ((not (negative? l2))
                                       (interval (* l1 l2)
                                                 (* u1 u2)))
                                      ((negative? u2)
                                       (interval (* u1 l2)
                                                 (* l1 u2)))
                                      (else
                                       (interval (* u1 l2)
                                                 (* u1 u2)))))
          ((negative? u1) (cond ((not (negative? l2))
                                 (interval (* l1 u2)
                                           (* u1 l2)))
                                ((negative? u2)
                                 (interval (* u1 u2)
                                           (* l1 l2)))
                                (else
                                 (interval (* l1 u2)
                                           (* l1 l2)))))
          (else (cond ((not (negative? l2))
                       (interval (* l1 u2)
                                 (* u1 u2)))
                      ((negative? u2)
                       (interval (* u1 l2)
                                 (* l1 l2)))
                      (else
                       (interval (min (* l1 u2) (* u1 l2))
                                 (max (* l1 l2) (* u1 u2)))))))))

(define (interval-div i1 i2)
  (let ((l2 (interval-lower i2))
        (u2 (interval-upper i2)))
    (if (or (zero? l2) (zero? u2) (and (negative? l2) (positive? u2)))
        (error "division by zero:" i1 i2)
        (interval-mul i2 (interval (/ 1.0 u2) (/ 1.0 l2))))))

(define (center i)
  (average (interval-lower i) (interval-upper i)))

(define (average . args)
  (/ (apply + args) (length args)))

(define (width i)
  (/ (- (interval-upper i) (interval-lower i)) 2.0))

(define (center-width c w)
  (interval (- c w) (+ c w)))

(define (percent i)
  (* (/ (width i) (center i)) 100.0))

(define (center-percent c p)
  (center-width c (* c (/ p 100.0))))