#lang sicp

(define f
  (let ((state 1))
    (lambda (x)
      (let ((prev state))
        (set! state x)
        (* prev x)))))

(f 1)
;; 1

(f 0)
;; 0

(f 1)
;; 0