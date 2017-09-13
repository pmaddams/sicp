#lang sicp

(define (fringe t)
  (letrec ((f (lambda (t result)
                (cond ((null? t)
                       result)
                      ((list? (car t))
                       (append (f (car t) '())
                               (f (cdr t) result)))
                      (else
                       (cons (car t)
                             (f (cdr t) result)))))))
    (f t '())))

(define (displayln x)
  (begin (display x)
         (newline)))

(let ((t '((1 2) (3 4))))
  (displayln (fringe t))
  (displayln (fringe (list t t))))
;; (1 2 3 4)
;; (1 2 3 4 1 2 3 4)
