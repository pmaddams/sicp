#lang sicp

(define (element-of-set? x set)
  (letrec ((e (lambda (set)
                (and (not (null? set))
                     (or (equal? x (car set))
                         (e (cdr set)))))))
    (e set)))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (letrec ((i (lambda (set1)
                (cond ((null? set1)
                       '())
                      ((element-of-set? (car set1) set2)
                       (cons (car set1)
                             (i (cdr set1))))
                      (else
                       (i (cdr set1)))))))
    (i set1)))

(define (union-set set1 set2)
  (letrec ((u (lambda (set1)
                (cond ((null? set1)
                       set2)
                      ((element-of-set? (car set1) set2)
                       (u (cdr set1)))
                      (else
                       (cons (car set1)
                             (u (cdr set1))))))))
    (u set1)))

(define (displayln x)
  (display x)
  (newline))

(let ((set1 '(a b c d e))
      (set2 '(c d e f g)))
  (for-each displayln
            (list (intersection-set set1 set2)
                  (union-set set1 set2))))
;; (c d e)
;; (a b c d e f g)
