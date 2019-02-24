#lang racket/base

; Exercise 2.70

(provide (all-defined-out))

(struct leaf (symbol weight))

(struct branch (symbols weight left right))

(define (make-branch left right)
  (branch (append (symbols left) (symbols right))
          (+ (weight left) (weight right))
          left
          right))

(define (symbols t)
  (if (leaf? t)
      (list (leaf-symbol t))
      (branch-symbols t)))

(define (weight t)
  (if (leaf? t)
      (leaf-weight t)
      (branch-weight t)))

(define (make-huffman-tree pairs)
  (if (< (length pairs) 2)
      (error "not enough symbols:" pairs)
      (merge (leaf-set pairs))))

(define (merge l)
  (let loop ((l (cddr l)) (acc (make-branch (car l) (cadr l))))
    (if (null? l)
        acc
        (loop (cdr l) (make-branch (car l) acc)))))

(define (leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin (leaf (car pair) (cdr pair))
                (leaf-set (cdr pairs))))))

(define (adjoin t set)
  (cond ((null? set) (list t))
        ((< (weight t) (weight (car set))) (cons t set))
        (else (cons (car set) (adjoin t (cdr set))))))

(define (encode message t)
  (let loop ((message message))
    (if (null? message)
        '()
        (append (encode-symbol (car message) t)
                (loop (cdr message))))))

(define (encode-symbol symbol t)
  (let loop ((t t) (acc '()))
    (cond ((leaf? t) (reverse acc))
          ((contains? symbol (branch-left t)) (loop (branch-left t) (cons 0 acc)))
          ((contains? symbol (branch-right t)) (loop (branch-right t) (cons 1 acc)))
          (else (error "unknown symbol:" symbol)))))

(define (contains? symbol t)
  (member symbol (symbols t)))

(define (decode bits t)
  (if (null? bits)
      '()
      (let ((next (choose-branch (car bits) t)))
        (if (leaf? next)
            (cons (leaf-symbol next) (decode (cdr bits) t))
            (decode (cdr bits) next)))))

(define (choose-branch bit t)
  (if (zero? bit)
      (branch-left t)
      (branch-right t)))

(define alphabet
  (make-huffman-tree
   '((A . 2)
     (BOOM . 1)
     (GET . 2)
     (JOB . 2)
     (NA . 16)
     (SHA . 3)
     (YIP . 9)
     (WAH . 1))))

(define song
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))
