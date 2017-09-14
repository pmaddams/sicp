#lang sicp

(define (foldr proc init seq)
  (letrec ((f (lambda (seq)
                (if (null? seq)
                    init
                    (proc (car seq)
                          (f (cdr seq)))))))
    (f seq)))

(define (foldl proc init seq)
  (letrec ((f (lambda (seq result)
                (if (null? seq)
                    result
                    (f (cdr seq)
                       (proc (car seq)
                             result))))))
    (f seq init)))

(define (reverse-a seq)
  (foldr (lambda (x y)
           (append y (list x)))
         '()
         seq))

(define (reverse-b seq)
  (foldl cons '() seq))

(define (displayln x)
  (display x)
  (newline))

(let ((seq '(1 2 3)))
  (displayln (reverse-a seq))
  (displayln (reverse-b seq)))
;; (3 2 1)
;; (3 2 1)
