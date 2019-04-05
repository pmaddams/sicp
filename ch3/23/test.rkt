#lang racket/base

(require racket/class
         rackunit
         "main.rkt")

(test-case
 "deque"
 (let ((d (new deque%)))
   (for ((i (in-range 1 6)))
     (send d push-front i))
   (for ((i (in-range 6 11)))
     (send d push-back i))
   (check-equal? (send d pop-front) 5)
   (check-equal? (send d pop-back) 10)
   (check-equal? (send d pop-front) 4)
   (check-equal? (send d pop-back) 9)
   (for ((i (in-range 6)))
     (send d pop-front))
   (check-true (with-handlers ((exn:fail? (lambda (e) #t)))
                 (send d pop-back)))))
