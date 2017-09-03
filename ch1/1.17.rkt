#lang sicp

(define (* a b)
  (let ((double (lambda (x)
                  (+ x x)))
        (halve (lambda (x)
                 (/ x 2))))
    (cond ((zero? b) 0)
          ((even? b) (* (double a) (halve b)))
          (else (+ a (* a (dec b)))))))

(* 2 3)
;; 6
