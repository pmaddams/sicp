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

(define (average-damp f)
  (let ((average (lambda args
                   (/ (apply + args)
                      (length args)))))
    (lambda (x)
      (average x (f x)))))

(define (repeated f n)
  (let ((double (lambda (f)
                  (lambda (x)
                    (f (f x))))))
    (if (= n 1)
        f
        (repeated (double f) (dec n)))))

;; TODO
