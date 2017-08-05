#lang sicp

(define (displayln x)
  (begin (display x)
         (newline)))

(define (fringe t)
  (letrec ((f (lambda (t l)
                (cond ((null? t) l)
                      ((list? (car t)) (append (f (car t) '()) (f (cdr t) l)))
                      (else (cons (car t) (f (cdr t) l)))))))
    (f t '())))

(let ((x '((1 2) (3 4))))
  (begin (displayln (fringe x))
         (displayln (fringe (list x x)))))
;; (1 2 3 4)
;; (1 2 3 4 1 2 3 4)