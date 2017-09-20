#lang sicp

(define (make-monitored f)
  (let* ((count 0)
         (inc-count (lambda ()
                      (set! count (inc count))))
         (reset-count (lambda ()
                        (set! count 0)))
         (dispatch (lambda (m)
                     (case m
                       ('how-many-calls? count)
                       ('reset-count (reset-count))
                       (else (inc-count)
                             (f m))))))
    dispatch))

(define (displayln x)
  (display x)
  (newline))

(let ((s (make-monitored sqrt)))
  (displayln (s 100))
  (displayln (s 'how-many-calls?))
  (s 'reset-count)
  (displayln (s 'how-many-calls?)))
;; 10
;; 1
;; 0
