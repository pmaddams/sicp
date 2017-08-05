#lang sicp

(define (deep-reverse l)
  (letrec ((r (lambda (l newl)
                (cond ((null? l) newl)
                      ((list? (car l)) (r (cdr l) (cons (r (car l) '()) newl)))
                      (else (r (cdr l) (cons (car l) newl)))))))
    (r l '())))

(let ((l '((1 2) (3 4))))
  (display (deep-reverse l)))
;; ((4 3) (2 1))