#lang sicp

(define (smallest-divisor n)
  (let ((square (lambda (x)
                  (expt x 2)))
        (divides? (lambda (a b)
                    (zero? (remainder b a)))))
    (letrec ((s (lambda (i)
                  (cond ((> (square i) n) n)
                        ((divides? i n) i)
                        (else (s (inc i)))))))
      (s 2))))

(smallest-divisor 199)
;; 199

(smallest-divisor 1999)
;; 1999

(smallest-divisor 19999)
;; 7
