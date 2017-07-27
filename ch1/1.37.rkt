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

(define (how-large-must-k-be? f)
  (letrec ((phi 1.6180)
           (one (lambda (x) 1.0))
           (ok? (lambda (x)
                  (< (abs (- (/ 1 x) phi))
                     0.00005)))
           (o (lambda (k)
                (if (ok? (f one one k))
                    k
                    (o (inc k))))))
    (o 1)))

(how-large-must-k-be? cont-frac-rec)
;; 11

(how-large-must-k-be? cont-frac-iter)
;; 11