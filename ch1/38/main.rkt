#lang racket/base

; Exercise 1.38: Continued fractions

(provide e-within
         pi-within
         within?)

(define (e-within percent)
  (let ((numer (lambda (i) 1))
        (denom (lambda (i)
                 (let ((j (add1 i)))
                   (if (zero? (modulo j 3))
                       (* 2 (/ j 3))
                       1)))))
    (+ 2.0 (cont-frac-within percent numer denom))))

(define (pi-within percent)
  (let ((numer (lambda (i)
                 (square (sub1 (* 2 i)))))
        (denom (lambda (i) 6)))
    (+ 3.0 (cont-frac-within percent numer denom))))

(define (square n)
  (* n n))

(define (cont-frac-within percent numer denom)
  (let loop ((guess (cont-frac numer denom 1)) (terms 2))
    (let ((next (cont-frac numer denom terms)))
      (if ((within? percent) guess next)
          next
          (loop next (* 2 terms))))))

(define (cont-frac numer denom terms)
  (let loop ((i terms) (acc 0))
    (if (zero? i)
        acc
        (loop (sub1 i) (/ (numer i)
                          (+ (denom i) acc))))))

(define (within? percent)
  (lambda (guess next)
    (< (abs (/ (- next guess) guess))
       (/ percent 100.0))))
