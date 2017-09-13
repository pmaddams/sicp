#lang sicp

(define (accumulate proc init seq)
  (letrec ((a (lambda (seq)
                (if (null? seq)
                    init
                    (proc (car seq)
                          (a (cdr seq)))))))
    (a seq)))

(define (accumulate-n proc init seqs)
  (let ((a (lambda (seq)
             (accumulate proc init seq))))
    (letrec ((an (lambda (seqs)
                   (if (null? (car seqs))
                       '()
                       (cons (a (map car seqs))
                             (an (map cdr seqs)))))))
      (an seqs))))

(let ((s '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
  (display (accumulate-n + 0 s)))
;; (22 26 30)
