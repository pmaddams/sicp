#lang sicp

(define (reverse l)
  (letrec ((r (lambda (l newl)
                (if (null? l)
                    newl
                    (r (cdr l) (cons (car l) newl))))))
    (r l '())))

(let ((l '(1 4 9 16 25)))
  (begin (display (reverse l))
         (newline)))
;; (25 16 9 4 1)