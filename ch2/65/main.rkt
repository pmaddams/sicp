#lang racket/base

; Exercise 2.65

(provide (all-defined-out))

(struct tree (val left right))

(define (list->tree l)
  (car (let loop ((l l) (n (length l)))
         (if (zero? n)
             (cons '() l)
             (let* ((left-size (quotient (sub1 n) 2))
                    (left-pair (loop l left-size))
                    (non-left-l (cdr left-pair))
                    (right-size (- n (add1 left-size)))
                    (right-pair (loop (cdr non-left-l) right-size)))
               (cons (tree (car non-left-l) (car left-pair) (car right-pair))
                     (cdr right-pair)))))))

(define (tree->list t)
  (let loop ((t t) (acc '()))
    (if (null? t)
        acc
        (loop (tree-left t)
              (cons (tree-val t)
                    (loop (tree-right t) acc))))))

(define (element? n t)
  (let loop ((t t))
    (and (not (null? t))
         (let ((val (tree-val t)))
           (cond ((< n val) (loop (tree-left t)))
                 ((> n val) (loop (tree-right t)))
                 (else val))))))

(define (adjoin n t)
  (let loop ((t t))
    (if (null? t)
        (tree n '() '())
        (let ((val (tree-val t)))
          (cond ((< n val) (tree val (loop (tree-left t)) (tree-right t)))
                ((> n val) (tree val (tree-right t) (loop (tree-right t))))
                (else t))))))

(define (union t1 t2)
  (list->tree
   (let loop ((l1 (tree->list t1)) (l2 (tree->list t2)))
     (cond ((null? l1) l2)
           ((null? l2) l1)
           (else (let ((n1 (car l1))
                       (n2 (car l2)))
                   (cond ((< n1 n2) (cons n1 (loop (cdr l1) l2)))
                         ((> n1 n2) (cons n2 (loop l1 (cdr l2))))
                         (else (cons n1 (loop (cdr l1) (cdr l2)))))))))))

(define (intersect t1 t2)
  (list->tree
   (let loop ((l1 (tree->list t1)) (l2 (tree->list t2)))
     (if (or (null? l1)
             (null? l2))
         '()
         (let ((n1 (car l1))
               (n2 (car l2)))
           (cond ((< n1 n2) (loop (cdr l1) l2))
                 ((> n1 n2) (loop l1 (cdr l2)))
                 (else (cons n1 (loop (cdr l1) (cdr l2))))))))))
