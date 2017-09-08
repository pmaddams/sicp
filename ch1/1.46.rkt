#lang sicp

(define (good-enough? guess next)
  (let ((tolerance 0.00001))
    (< (abs (/ (- guess next) next))
       tolerance)))

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (letrec ((i (lambda (guess)
                  (let ((next (improve guess)))
                    (if (good-enough? guess next)
                        next
                        (i next))))))
      (i guess))))

(define (average . args)
  (/ (apply + args)
     (length args)))

(define (sqrt x)
  (let ((improve (lambda (guess)
                   (average guess (/ x guess)))))
    ((iterative-improve good-enough? improve) 1.0)))

(sqrt (expt 1000000 2))
;; 1000000.0000000054

(define (fixed-point f guess)
  ((iterative-improve good-enough? f) guess))

(fixed-point cos 1.0)
;; 0.7390822985224024
