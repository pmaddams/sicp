#lang sicp

(define (accumulate proc init seq)
  (letrec ((a (lambda (seq)
                (if (null? seq)
                    init
                    (proc (car seq)
                          (a (cdr seq)))))))
    (a seq)))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (st)
                     (if (not (pair? st))
                         1
                         (count-leaves st)))
                   t)))

(define (displayln x)
  (display x)
  (newline))

(let ((t '((1 2) (3 4))))
  (displayln (count-leaves t))
  (displayln (count-leaves (list t t))))
;; 4
;; 8
