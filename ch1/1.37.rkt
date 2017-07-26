#lang sicp

(define (cont-frac-rec n d k)
  (letrec ((c (lambda (i)
                (if (> i k)
                    0
                    (/ (n i)
                       (+ (d i) (c (inc i))))))))
    (c 1)))

(define (cont-frac-iter n d k)
  (letrec ((c (lambda (i result)
                (if (zero? i)
                    result
                    (c (dec i) (/ (n i)
                                  (+ (d i) result)))))))
    (c k 0)))