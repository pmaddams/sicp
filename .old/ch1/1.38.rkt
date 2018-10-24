#lang sicp

(define (cont-frac n d k)
  (letrec ((c (lambda (i result)
                (if (zero? i)
                    result
                    (c (dec i)
                       (/ (n i)
                          (+ result (d i))))))))
    (c k 0)))

(define (e-cf k)
  (let ((n (lambda (i) 1.0))
        (d (lambda (i)
             (let ((j (- i 2)))
               (if (zero? (modulo j 3))
                   (* 2.0 (inc (/ j 3)))
                   1.0)))))
    (+ 2 (cont-frac n d k))))

(exp 1)
;; 2.718281828459045

(e-cf 20)
;; 2.718281828459045