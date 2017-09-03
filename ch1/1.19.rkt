#lang sicp

;; a' = b*q + a*q + a*p
;;    = (p + q)*a + q*b
;; 
;; b' = b*p + a*q
;;    = q*a + p*b
;; 
;; a" = (p + q)*a' + q*b'
;;    = (p + q)*((p + q)*a + q*b) + q*(q*a + p*b)
;;    = (p + q)^2*a + (p + q)*q*b + q^2*a + p*q*b
;;    = p^2*a + 2*p*q*a + q^2*a + p*q*b + q^2*b + q^2*a + p*q*b
;;    = (p^2 + 2*p*q + 2*q^2)*a + (2*p*q + q^2)*b
;; 
;; b" = q*a' + p*b'
;;    = q*((p + q)*a + q*b) + p*(q*a + p*b)
;;    = p*q*a + q^2*a + q^2*b + p*q*a + p^2*b
;;    = (2*p*q + q^2)*a + (p^2 + q^2)*b
;; 
;; Therefore p' = p^2 + q^2 and q' = 2*p*q + q^2.

(define (fib n)
  (let* ((square (lambda (x)
                   (expt x 2)))
         (next-p (lambda (p q)
                   (+ (square p)
                      (square q))))
         (next-q (lambda (p q)
                   (+ (* 2 p q)
                      (square q)))))
    (letrec ((f (lambda (a b p q count)
                  (cond ((zero? count) b)
                        ((even? count) (f a
                                          b
                                          (next-p p q)
                                          (next-q p q)
                                          (/ count 2)))
                        (else (f (+ (* b q) (* a q) (* a p))
                                 (+ (* b p) (* a q))
                                 p
                                 q
                                 (dec count)))))))
      (f 1 0 0 1 n))))

(fib 10)
;; 55
