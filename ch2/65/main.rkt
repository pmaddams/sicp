#lang racket/base

; Exercise 2.65: Set representation

(struct tree (val left right))

(define (element? x t)
  (letrec ((e (lambda (t)
                (cond ((null? t)
                       #f)
                      ((< x (tree-val t))
                       (e (tree-left t)))
                      ((> x (tree-val t))
                       (e (tree-right t)))
                      (else
                       #t)))))
    (e t)))

(define (adjoin x t)
  (letrec ((a (lambda (t)
                (cond ((null? t)
                       (tree x '() '()))
                      ((< x (tree-val t))
                       (tree (tree-val t)
                             (a (tree-left t))
                             (tree-right t)))
                      ((> x (tree-val t))
                       (tree (tree-val t)
                             (tree-left t)
                             (a (tree-right t))))
                      (else t)))))
    (a t)))

(define (tree->list t)
  (letrec ((tl (lambda (t result)
                 (if (null? t)
                     result
                     (tl (tree-left t)
                         (cons (tree-val t)
                               (tl (tree-right t)
                                   result)))))))
    (tl t '())))

(define (list->tree l)
  (letrec ((lt (lambda (l n)
                 (if (zero? n)
                     (cons '() l)
                     (let* ((left-size (quotient (sub1 n) 2))
                            (right-size (- n (add1 left-size)))
                            (left-result (lt l
                                             left-size))
                            (left-tree (car left-result))
                            (non-left-l (cdr left-result))
                            (this-tree-val (car non-left-l))
                            (right-result (lt (cdr non-left-l)
                                              right-size))
                            (right-tree (car right-result))
                            (remaining-l (cdr right-result)))
                       (cons (tree this-tree-val
                                   left-tree
                                   right-tree)
                             remaining-l))))))
    (car (lt l (length l)))))

(define (intersection t1 t2)
  (letrec ((i (lambda (l result)
                (cond ((null? l)
                       (list->tree result))
                      ((element? (car l) t2)
                       (i (cdr l)
                          (append result
                                  (list (car l)))))
                      (else
                       (i (cdr l)
                          result))))))
    (i (tree->list t1) '())))

(define (union t1 t2)
  (letrec ((u (lambda (l result)
                (cond ((null? l)
                       (list->tree (append result
                                           (tree->list t2))))
                      ((element? (car l) t2)
                       (u (cdr l)
                          result))
                      (else
                       (u (cdr l)
                          (append result
                                  (list (car l)))))))))
    (u (tree->list t1) '())))
