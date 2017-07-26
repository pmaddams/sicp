#lang sicp

(define (fixed-point f guess)
  (letrec ((tolerance 0.00001)
           (close-enough? (lambda (a b)
                            (< (abs (- a b)) tolerance)))
           (try (lambda (guess)
                  (let ((next (f guess)))
                    (if (close-enough? guess next)
                        next
                        (try next))))))
    (try guess)))

;; x = 1 + 1 / x
;; x^2 - x - 1 = 0

;; By the quadratic formula:

;; x = (1 +- sqrt(1 - 4 * (1 * -1))) / 2
;; x = (1 +- sqrt(5)) / 2

;; The positive solution,

;; (1 + sqrt(5)) / 2

;; is equal to phi.

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;; 1.6180327868852458