#lang sicp

(define (fast-* a b)
  (let ((double (lambda (x)
                  (* x 2)))
        (halve (lambda (x)
                 (/ x 2))))
    (cond ((zero? b) 0)
          ((even? b) (double (fast-* a (halve b))))
          (else (+ a (fast-* a (dec b)))))))