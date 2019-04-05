#lang racket/base

(require compatibility/mlist
         rackunit
         "main.rkt")

(test-case
 "has-cycle?"
 (for ((ml (in-list `(()
                      ,(mlist 0)
                      ,(mlist 0 0)
                      ,(mlist 0 1 (mlist 0 1))))))
   (check-false (has-cycle? ml)))
 (let ((ml (mlist 0 1)))
   (set-mcdr! (mcdr ml) ml)
   (check-true (has-cycle? ml))))
