#lang sicp

(define (monte-carlo trials experiment?)
  (letrec ((m (lambda (remaining passed)
                (cond ((zero? remaining)
                       (/ passed trials))
                      ((experiment?)
                       (m (dec remaining)
                          (inc passed)))
                      (else
                       (m (dec remaining)
                          passed))))))
    (m trials 0)))

(define (rand)
  (random #x7fffffff))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (estimate-pi-a trials)
  (sqrt (/ 6
           (monte-carlo trials
                        cesaro-test))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (square x)
  (expt x 2))

(define (circle-test x1 x2 y1 y2)
  (let* ((x-dist (- x2 x1))
         (y-dist (- y2 y1))
         (radius (if (not (= x-dist y-dist))
                     (error "invalid coordinates")
                     (/ x-dist 2)))
         (x-center (+ x1 radius))
         (y-center (+ y1 radius)))
    (lambda ()
      (let ((x-test (random-in-range x1 x2))
            (y-test (random-in-range y1 y2)))
        (<= (+ (square (- x-test x-center))
               (square (- y-test y-center)))
            (square radius))))))

(define (estimate-integral circle-test x1 x2 y1 y2 trials)
  (* (monte-carlo trials (circle-test x1 x2 y1 y2))
     4.0))

(define (estimate-pi-b trials)
  (estimate-integral circle-test -10.0 10.0 -10.0 10.0 trials))

(define (displayln x)
  (display x)
  (newline))

(for-each (lambda (estimate)
            (displayln (estimate 100000)))
          (list estimate-pi-a
                estimate-pi-b))
;; 3.1452606552092153
;; 3.14244
