#lang sicp

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (count-primes p)
  (lambda (n)
    (letrec ((c (lambda (n result)
                  (if (= (gcd n p) 1)
                      result
                      (c (/ n p)
                         (inc result))))))
      (c n 0))))

(define car (count-primes 2))

(define cdr (count-primes 3))

(define (displayln x)
  (display x)
  (newline))

(let ((n (cons 123 456)))
  (displayln (car n))
  (displayln (cdr n)))
;; 123
;; 456
