#lang sicp

(define (accumulate proc init seq)
  (letrec ((a (lambda (seq)
                (if (null? seq)
                    init
                    (proc (car seq)
                          (a (cdr seq)))))))
    (a seq)))

(define (map proc seq)
  (accumulate (lambda (x y)
                (cons (proc x) y))
              '()
              seq))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y)
                (inc y))
              0
              seq))

(define (square x)
  (expt x 2))

(define (displayln x)
  (display x)
  (newline))

(let ((seq1 '(1 2 3))
      (seq2 '(4 5 6 7)))
  (displayln (map square seq1))
  (displayln (append seq1 seq2))
  (displayln (length seq2)))
;; (1 4 9)
;; (1 2 3 4 5 6 7)
;; 4
