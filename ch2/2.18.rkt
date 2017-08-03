#lang sicp

(define (reverse l)
  (letrec ((r (lambda (l1 l2)
                (if (null? l1)
                    l2
                    (r (cdr l1) (cons (car l1) l2))))))
    (r l '())))

(let ((l '(1 4 9 16 25)))
  (display (reverse l)))
;; (25 16 9 4 1)