#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car z)
  (letrec ((c (lambda (z result)
                (if (= 1 (gcd z 2))
                    result
                    (c (/ z 2) (inc result))))))
    (c z 0)))

(define (cdr z)
  (letrec ((c (lambda (z result)
                (if (= 1 (gcd z 3))
                    result
                    (c (/ z 3) (inc result))))))
    (c z 0)))

(let ((p (cons 123 456)))
  (begin (display (car p))
         (newline)
         (display (cdr p))
         (newline)))
;; 123
;; 456