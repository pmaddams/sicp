#lang sicp

(define (fixed-point f guess)
  (let* ((tolerance 0.00001)
         (close-enough? (lambda (a b)
                          (< (abs (- a b)) tolerance))))
    (letrec ((try (lambda (guess)
                    (let ((next (f guess)))
                      (if (close-enough? guess next)
                          next
                          (try next))))))
      (try guess))))

(define (deriv g)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (- (g (+ x dx))
            (g x))
         dx))))

(define (newton-transform g)
  (lambda (x)
    (- x
       (/ (g x)
          ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (expt x 3)
       (* a (expt x 2))
       (* b x)
       c)))

(newtons-method (cubic 2 3 2) 1.0)
;; -0.9999999999961774
