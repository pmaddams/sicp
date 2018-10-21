#lang sicp

(define (make-monitored proc)
  (let* ((count 0)
         (inc-count (lambda ()
                      (set! count
                            (inc count))))
         (reset-count (lambda ()
                        (set! count
                              0)
                        count))
         (dispatch (lambda (m)
                     (case m
                       ('how-many-calls? count)
                       ('reset-count (reset-count))
                       (else (inc-count)
                             (proc m))))))
    dispatch))

(define (displayln x)
  (display x)
  (newline))

(let ((s (make-monitored sqrt)))
  (for-each (lambda (m)
              (displayln (s m)))
            '(100
              how-many-calls?
              reset-count)))
;; 10
;; 1
;; 0
