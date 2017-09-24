#lang sicp

(define f
  (let ((state 1))
    (lambda (x)
      (let ((prev state))
        (set! state x)
        (* prev x)))))

(define (displayln x)
  (display x)
  (newline))

(for-each (lambda (x)
            (displayln (f x)))
          '(1 0 1))
;; 1
;; 0
;; 0
