#lang racket/base

; Exercise 2.70

(provide (all-defined-out))

(require racket/list)

(struct leaf (symbol weight) #:transparent)

(struct branch (symbols weight left right) #:transparent)

(define (make-huffman-tree symbols)
  (make-branches (make-leaves (counts symbols))))

(define (make-branches set)
  (unless (> (length set) 1)
    (error "not enough information:" set))
  (let loop ((set set))
    (if (null? (cdr set))
        (car set)
        (loop (insert (make-branch (car set) (cadr set))
                      (cddr set))))))

(define (make-branch left right)
  (branch (append (symbols left) (symbols right))
          (+ (weight left) (weight right))
          left
          right))

(define (make-leaves pairs)
  (foldr insert '() (map make-leaf pairs)))

(define (make-leaf pair)
  (leaf (car pair) (cdr pair)))

(define (encode symbols t)
  (append-map (lambda (s) (encode-symbol s t)) symbols))

(define (encode-symbol s t)
  (if (leaf? t)
      '()
      (let ((left (branch-left t))
            (right (branch-right t)))
        (cond ((contains? s left) (cons 0 (encode-symbol s left)))
              ((contains? s right) (cons 1 (encode-symbol s right)))
              (else (error "unknown symbol:" s))))))

(define (contains? s t)
  (memq s (symbols t)))

(define (decode bits root)
  (let loop ((bits bits) (current root))
    (if (null? bits)
        '()
        (let ((next (choose-branch (car bits) current)))
          (if (leaf? next)
              (cons (leaf-symbol next) (loop (cdr bits) root))
              (loop (cdr bits) next))))))

(define (choose-branch bit t)
  (cond ((= bit 0) (branch-left t))
        ((= bit 1) (branch-right t))
        (else (error "invalid bit:" bit))))

(define (insert t set)
  (cond ((null? set) (list t))
        ((< (weight t) (weight (car set))) (cons t set))
        (else (cons (car set) (insert t (cdr set))))))

(define (symbols t)
  (if (leaf? t)
      (list (leaf-symbol t))
      (branch-symbols t)))

(define (weight t)
  (if (leaf? t)
      (leaf-weight t)
      (branch-weight t)))

(define (counts symbols)
  (for/list ((s (in-list (remove-duplicates symbols))))
    (let ((p (lambda (x) (eq? s x))))
      (cons s (length (filter p symbols))))))

(define get-a-job
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))
