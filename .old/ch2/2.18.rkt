#lang sicp

(define (reverse l)
  (letrec ((r (lambda (l result)
                (if (null? l)
                    result
                    (r (cdr l)
                       (cons (car l) result))))))
    (r l '())))

(let ((l '(1 4 9 16 25)))
  (display (reverse l)))
;; (25 16 9 4 1)
