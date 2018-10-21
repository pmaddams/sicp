#lang sicp

(define (cont-frac-rec n d k)
  (letrec ((c (lambda (i)
                (if (> i k)
                    0
                    (/ (n i)
                       (+ (c (inc i)) (d i)))))))
    (c 1)))

(define (cont-frac-iter n d k)
  (letrec ((c (lambda (i result)
                (if (zero? i)
                    result
                    (c (dec i)
                       (/ (n i)
                          (+ result (d i))))))))
    (c k 0)))

(define (how-large-must-k-be? cont-frac)
  (let* ((one (lambda (x) 1.0))
         (phi 1.6180)
         (tolerance 0.00005)
         (good-enough? (lambda (x)
                         (< (abs (- (/ 1 x) phi))
                            tolerance))))
    (letrec ((h (lambda (k)
                  (if (good-enough? (cont-frac one one k))
                      k
                      (h (inc k))))))
      (h 1))))

(how-large-must-k-be? cont-frac-rec)
;; 11

(how-large-must-k-be? cont-frac-iter)
;; 11
