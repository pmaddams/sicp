#lang sicp

(define (fringe t)
  (letrec ((f (lambda (t l)
                (cond ((null? t) l)
                      ((list? (car t)) (append (f (car t) '())
                                               (f (cdr t) l)))
                      (else (cons (car t) (f (cdr t) l)))))))
    (f t '())))

(define (displayln x)
  (begin (display x)
         (newline)))

(let ((x '((1 2) (3 4))))
  (displayln (fringe x))
  (displayln (fringe (list x x))))
;; (1 2 3 4)
;; (1 2 3 4 1 2 3 4)