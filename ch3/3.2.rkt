#lang sicp

(define (make-monitored f)
  (let* ((count 0)
         (inc-count (lambda ()
                      (set! count (inc count))))
         (reset-count (lambda ()
                        (set! count 0)))
         (dispatch (lambda (m)
                     (cond ((eq? m 'how-many-calls?) count)
                           ((eq? m 'reset-count) (reset-count))
                           (else (inc-count)
                                 (f m))))))
    dispatch))