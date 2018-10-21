#lang sicp

(define (foldr proc init l)
  (letrec ((f (lambda (l)
                (if (null? l)
                    init
                    (proc (car l)
                          (f (cdr l)))))))
    (f l)))

(define (foldr-n proc init ls)
  (let ((f (lambda (l)
             (foldr proc init l))))
    (letrec ((fn (lambda (ls)
                   (if (null? (car ls))
                       '()
                       (cons (f (map car ls))
                             (fn (map cdr ls)))))))
      (fn ls))))

(let ((s '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
  (display (foldr-n + 0 s)))
;; (22 26 30)
