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

(define (average . args)
  (/ (apply + args)
     (length args)))

(define (average-damp f)
  (lambda (x)
      (average x (f x))))

(define (double f)
  (lambda (x)
    (f (f x))))

(define (repeated f n)
  (if (= n 1)
      f
      (repeated (double f) (dec n))))

(define (nth-root n)
  (lambda (x)
    (fixed-point ((repeated average-damp (ceiling (log n)))
                  (lambda (y)
                    (/ x (expt y (dec n)))))
                 1.0)))

(define (displayln x)
  (display x)
  (newline))

(define (enumerate-interval low high)
  (letrec ((e (lambda (i result)
                (if (< i low)
                    result
                    (e (dec i)
                       (cons i result))))))
    (e high '())))

(for-each displayln (map (lambda (n)
                           ((nth-root n) (expt n n)))
                         (enumerate-interval 2 10)))
;; 2.000000000000002
;; 3.000001464168659
;; 4.000000000000006
;; 5.0000017463386754
;; 6.000002917879925
;; 6.999996178069843
;; 8.000006144851259
;; 9.000006400477798
;; 10.000003898325836
