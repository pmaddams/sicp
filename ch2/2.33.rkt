#lang sicp

(define (accumulate op init seq)
  (letrec ((a (lambda (seq)
                (if (null? seq)
                    init
                    (op (car seq) (a (cdr seq)))))))
    (a seq)))

(define (map p seq)
  (accumulate (lambda (x y)
                (cons (p x) y))
              '()
              seq))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y)
                (inc y))
              0
              seq))

(define (displayln x)
  (display x)
  (newline))

(let ((square (lambda (x)
                (expt x 2)))
      (seq '(1 2 3 4 5)))
  (displayln (map square seq)))
;; (1 4 9 16 25)

(let ((seq1 '(1 2 3 4 5))
      (seq2 '(6 7 8 9 10)))
  (displayln (append seq1 seq2)))
;; (1 2 3 4 5 6 7 8 9 10)

(let ((seq '(a b c d e)))
  (displayln (length seq)))
;; 5