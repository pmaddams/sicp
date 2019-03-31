#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "make-center-percent"
 (check-equal? (make-center-percent 5.0 10.0)
               (interval 4.5 5.5))
 (check-equal? (make-center-percent 10.0 20.0)
               (interval 8.0 12.0))
 (check-equal? (make-center-percent -20.0 50.0)
               (interval -30.0 -10.0)))

(test-case
 "make-center-width"
 (check-equal? (make-center-width 5.0 0.5)
               (interval 4.5 5.5))
 (check-equal? (make-center-width 10.0 2.0)
               (interval 8.0 12.0))
 (check-equal? (make-center-width -20.0 10.0)
               (interval -30.0 -10.0)))

(test-case
 "interval-add"
 (for ((i (in-range 5)))
   (let* ((c1 (random 5 10))
          (c2 (random 5 10))
          (w1 (random 1 c1))
          (w2 (random 1 c2)))
     (check-equal? (interval-add (make-center-width c1 w1)
                                 (make-center-width c2 w2))
                   (make-center-width (+ c1 c2)
                                      (+ w1 w2))))))

(test-case
 "interval-sub"
 (for ((i (in-range 5)))
   (let* ((c1 (random 5 10))
          (c2 (random 5 10))
          (w1 (random 1 c1))
          (w2 (random 1 c2)))
     (check-equal? (interval-sub (make-center-width c1 w1)
                                 (make-center-width c2 w2))
                   (make-center-width (- c1 c2)
                                      (+ w1 w2))))))

(test-case
 "interval-mul"
 (let ((i1 (interval -10.0 -5.0))
       (i2 (interval 5.0 10.0)))
   (check-equal? (interval-mul i1 i1)
                 (interval 25.0 100.0))
   (check-equal? (interval-mul i1 i2)
                 (interval -100.0 -25.0))
   (check-equal? (interval-mul i2 i1)
                 (interval -100.0 -25.0))
   (check-equal? (interval-mul i2 i2)
                 (interval 25.0 100.0))))

(test-case
 "interval-div"
 (let ((i1 (interval -10.0 -5.0))
       (i2 (interval 5.0 10.0)))
   (check-equal? (interval-div i1 i1)
                 (interval 0.5 2.0))
   (check-equal? (interval-div i1 i2)
                 (interval -2.0 -0.5))
   (check-equal? (interval-div i2 i1)
                 (interval -2.0 -0.5))
   (check-equal? (interval-div i2 i2)
                 (interval 0.5 2.0))))
