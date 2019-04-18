#lang racket/base

; Exercise 2.12

(provide (all-defined-out))

(struct interval (lo hi) #:transparent)

(define (make-center-percent c %)
  (validate
   (let ((w (abs (* 0.01 % c))))
     (make-center-width c w))))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))

(define (make-center-width c w)
  (validate
   (interval (- c w) (+ c w))))

(define (center i)
  (average (interval-lo i) (interval-hi i)))

(define (width i)
  (/ (- (interval-hi i) (interval-lo i)) 2.0))

(define (interval-add i1 i2)
  (validate
   (interval (+ (interval-lo i1) (interval-lo i2))
             (+ (interval-hi i1) (interval-hi i2)))))

(define (interval-sub i1 i2)
  (validate
   (interval (- (interval-lo i1) (interval-hi i2))
             (- (interval-hi i1) (interval-lo i2)))))

(define (interval-mul i1 i2)
  (validate
   (let ((l1 (interval-lo i1))
         (l2 (interval-lo i2))
         (h1 (interval-hi i1))
         (h2 (interval-hi i2)))
     (cond ((not (negative? l1))
            (cond ((not (negative? l2))
                   (interval (* l1 l2) (* h1 h2)))
                  ((negative? h2)
                   (interval (* h1 l2) (* l1 h2)))
                  (else
                   (interval (* h1 l2) (* h1 h2)))))
           ((negative? h1)
            (cond ((not (negative? l2))
                   (interval (* l1 h2) (* h1 l2)))
                  ((negative? h2)
                   (interval (* h1 h2) (* l1 l2)))
                  (else
                   (interval (* l1 h2) (* l1 l2)))))
           (else
            (cond ((not (negative? l2))
                   (interval (* l1 h2) (* h1 h2)))
                  ((negative? h2)
                   (interval (* h1 l2) (* l1 l2)))
                  (else
                   (interval (min (* l1 h2) (* h1 l2))
                             (max (* l1 l2) (* h1 h2))))))))))

(define (interval-div i1 i2)
  (validate
   (let ((l2 (interval-lo i2))
         (h2 (interval-hi i2)))
     (if (or (zero? l2)
             (zero? h2)
             (and (negative? l2) (positive? h2)))
         (error "division by zero:" i1 i2)
         (interval-mul i1 (interval (/ 1.0 h2) (/ 1.0 l2)))))))

(define (validate i)
  (if (> (interval-lo i) (interval-hi i))
      (error "invalid interval:" i)
      i))

(define (average . args)
  (/ (apply + args)
     (length args)))
