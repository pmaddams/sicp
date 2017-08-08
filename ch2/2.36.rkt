#lang sicp

(define (accumulate op init seq)
  (letrec ((a (lambda (seq)
                (if (null? seq)
                    init
                    (op (car seq) (a (cdr seq)))))))
    (a seq)))

(define (accumulate-n op init seqs)
  (letrec ((a (lambda (seqs)
                (if (null? (car seqs))
                    '()
                    (cons (accumulate op init (map car seqs))
                          (a (map cdr seqs)))))))
    (a seqs)))

(let ((s '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
  (display (accumulate-n + 0 s)))