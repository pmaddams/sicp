#lang sicp

(define (sine angle)
  (let ((p (lambda (x)
             (- (* 3 x)
                (* 4 (expt x 3))))))
    (if (<= (abs angle) 0.1)
        angle
        (p (sine (/ angle 3.0))))))

(sine 12.15)
;; -0.3998034574133398

;; (p (sine 4.05))
;; (p (p (sine 1.35)))
;; (p (p (p (sine 0.45))))
;; (p (p (p (p (sine 0.15)))))
;; (p (p (p (p (p (sine 0.05))))))
;; (p (p (p (p (p (0.05))))))

;; When evaluating (sine 12.15), p is applied 5 times.

;; The order of growth with respect to both space and time is theta of log(a).
