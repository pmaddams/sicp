#lang racket/base

; Exercise 2.65

(provide (all-defined-out))

(require racket/list)

(struct node (val left right))

(define (set . args)
  (list->set args))

(define (set? x)
  (or (null? x)
      (node? x)))

(define (list->set l)
  (define (loop l n)
    (if (zero? n)
        (cons '() l)
        (let* ((left-size (quotient (sub1 n) 2))
               (left-pair (loop l left-size))
               (non-left-l (cdr left-pair))
               (right-size (- n (add1 left-size)))
               (right-pair (loop (cdr non-left-l) right-size)))
          (cons (node (car non-left-l) (car left-pair) (car right-pair))
                (cdr right-pair)))))

  (let ((l (normalize l)))
    (car (loop l (length l)))))

(define (set->list s)
  (let loop ((acc '()) (s s))
    (if (null? s)
        acc
        (loop (cons (node-val s)
                    (loop acc (node-right s)))
              (node-left s)))))

(define (set-member? s n)
  (let loop ((s s))
    (and (not (null? s))
         (let ((val (node-val s)))
           (cond ((< n val) (loop (node-left s)))
                 ((> n val) (loop (node-right s)))
                 (else #t))))))

(define (set-add s n)
  (let loop ((s s))
    (if (null? s)
        (node n '() '())
        (let ((val (node-val s)))
          (cond ((< n val) (node val (loop (node-left s)) (node-right s)))
                ((> n val) (node val (node-left s) (loop (node-right s))))
                (else s))))))

(define (set-union s1 s2)
  (list->set
   (let loop ((l1 (set->list s1)) (l2 (set->list s2)))
     (cond ((null? l1) l2)
           ((null? l2) l1)
           (else (let ((n1 (car l1))
                       (n2 (car l2)))
                   (cond ((< n1 n2) (cons n1 (loop (cdr l1) l2)))
                         ((> n1 n2) (cons n2 (loop l1 (cdr l2))))
                         (else (cons n1 (loop (cdr l1) (cdr l2)))))))))))

(define (set-intersect s1 s2)
  (list->set
   (let loop ((l1 (set->list s1)) (l2 (set->list s2)))
     (if (or (null? l1)
             (null? l2))
         '()
         (let ((n1 (car l1))
               (n2 (car l2)))
           (cond ((< n1 n2) (loop (cdr l1) l2))
                 ((> n1 n2) (loop l1 (cdr l2)))
                 (else (cons n1 (loop (cdr l1) (cdr l2))))))))))

(define (normalize l)
  (for ((n (in-list l)))
    (unless (number? n)
      (error "not a number:" n)))
  (sort (remove-duplicates l) <))
