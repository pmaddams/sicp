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

(define (newtons-method g guess)
  (let* ((dx 0.00001)
         (g-prime (lambda (x)
                    (/ (- (g (+ x dx)) (g x))
                       dx)))
         (f (lambda (x)
              (- x (/ (g x)
                      (g-prime x))))))
    (fixed-point f guess)))

(define (cubic a b c)
  (lambda (x)
    (+ (expt x 3)
       (* a (expt x 2))
       (* b x)
       c)))

(let ((a 2)
      (b 3)
      (c 2))
  (newtons-method (cubic a b c) 1.0))
;; -0.9999999999961774