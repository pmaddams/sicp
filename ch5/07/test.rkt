#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "expt"
 (for ((code (in-list
              (list recursive-process
                    iterative-process))))
   (let ((f (make-expt code)))
     (for ((i (in-range 5)))
       (let ((a (random 1 10))
             (n (random 1 10)))
         (check-equal? (f a n) (expt a n)))))))
