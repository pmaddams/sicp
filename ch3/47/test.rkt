#lang racket/base

(require racket/class
         rackunit
         "main.rkt")

(test-case
 "unprotected concurrency"
 (let* ((t (current-thread)))
   (concurrent
    (for ((i (in-range 5)))
      (thread-send t i)
      (sleep 0))
    (for ((j (in-range 5)))
      (thread-send t j)
      (sleep 0))))
 (let* ((l1 (for/list ((i (in-range 10)))
              (thread-receive)))
        (l2 (append (for/list ((i (in-range 5))) i)
                    (for/list ((j (in-range 5))) j))))
   (check-false (equal? l1 l2))))

(test-case
 "protected concurrency"
 (let* ((t (current-thread))
        (m (new mutex%)))
   (concurrent
    (begin (acquire m)
           (for ((i (in-range 5)))
             (thread-send t i)
             (sleep 0)))
    (begin (acquire m)
           (for ((j (in-range 5)))
             (thread-send t j)
             (sleep 0))))
   (let* ((l1 (append
               (let ((l (for/list ((i (in-range 5)))
                          (thread-receive))))
                 (release m)
                 l)
               (let ((l (for/list ((j (in-range 5)))
                          (thread-receive))))
                 (release m)
                 l)))
          (l2 (append (for/list ((i (in-range 5))) i)
                      (for/list ((j (in-range 5))) j))))
     (check-equal? l1 l2))))
