#lang sicp

(define (equal? l1 l2)
  (cond ((or (null? l1)
             (null? l2))
         (and (null? l1)
              (null? l2)))
        ((not (pair? (car l1)))
         (and (eq? (car l1) (car l2))
              (equal? (cdr l1) (cdr l2))))
        (else (and (equal? (car l1) (car l2))
                   (equal? (cdr l1) (cdr l2))))))

(equal? '(this is a list) '(this is a list))
;; #t

(equal? '(this is a list) '(this (is a) list))
;; #f