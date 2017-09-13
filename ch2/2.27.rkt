#lang sicp

(define (deep-reverse l)
  (letrec ((r (lambda (l result)
                (cond ((null? l)
                       result)
                      ((list? (car l))
                       (r (cdr l)
                          (cons (r (car l)
                                   '())
                                result)))
                      (else
                       (r (cdr l)
                          (cons (car l)
                                result)))))))
    (r l '())))

(define (displayln x)
  (display x)
  (newline))

(let ((l '((1 2) (3 4))))
  (displayln (reverse l))
  (displayln (deep-reverse l)))
;; ((3 4) (1 2))
;; ((4 3) (2 1))
