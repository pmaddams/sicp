#lang sicp

(define (fold-right op init seq)
  (letrec ((f (lambda (seq)
                (if (null? seq)
                    init
                    (op (car seq) (f (cdr seq)))))))
    (f seq)))

(define (fold-left op init seq)
  (letrec ((f (lambda (result rest)
                (if (null? rest)
                    result
                    (f (op result (car rest))
                       (cdr rest))))))
    (f init seq)))

(define (reverse-a seq)
  (let ((p (lambda (x y)
             (append y (list x)))))
    (fold-right p '() seq)))

(define (reverse-b seq)
  (let ((p (lambda (x y)
             (cons y x))))
    (fold-left p '() seq)))

(define (displayln x)
  (display x)
  (newline))

(let ((seq '(1 2 3)))
  (displayln (reverse-a seq))
  (displayln (reverse-b seq)))
;; (3 2 1)
;; (3 2 1)