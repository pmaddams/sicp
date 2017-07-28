#lang sicp

(define (repeated f n)
  (let ((double (lambda (f)
                  (lambda (x)
                    (f (f x))))))
    (if (= n 1)
        f
        (repeated (double f) (dec n)))))

(define (smooth f)
  (let ((dx 0.00001)
        (average (lambda args
                   (/ (apply + args)
                      (length args)))))
    (lambda (x)
      (average (f (- x dx))
               (f x)
               (f (+ x dx))))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(let ((four-fold-smoothed-cos
       (n-fold-smooth cos 4)))
  (four-fold-smoothed-cos 0.0))
;; 0.9999999997333333

(let ((four-fold-smoothed-sin
       (n-fold-smooth sin 4)))
  (four-fold-smoothed-sin 0.0))
;; 0.0