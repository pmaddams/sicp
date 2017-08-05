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

(define (nth-root n)
  (lambda (x)
    (fixed-point ((repeated average-damp (ceiling (log n)))
                  (lambda (y) (/ x (expt y (dec n)))))
                 1.0)))

(do ((n 2 (inc n))
     (lim 10))
  ((> n lim))
  (display ((nth-root n) (expt n n)))
  (newline))
;; 2.000000000000002
;; 3.000001464168659
;; 4.000000000000006
;; 5.0000017463386754
;; 6.000002917879925
;; 6.999996178069843
;; 8.000006144851259
;; 9.000006400477798
;; 10.000003898325836
