#lang racket/base

; Exercise 2.70

(provide (all-defined-out))

(require racket/list)

(struct leaf (symbol weight) #:transparent)

(struct branch (symbols weight left right) #:transparent)

(define (make-huffman-tree symbols)
  (make-branches (make-leaves (counts symbols))))

(define (make-branches l)
  (unless (> (length l) 1)
    (error "not enough information:" l))
  (let loop ((l l))
    (if (null? (cdr l))
        (car l)
        (loop (insert (make-branch (car l) (cadr l))
                      (cddr l))))))

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
      (let ((l (branch-left t))
            (r (branch-right t)))
        (cond ((contains? s l) (cons 0 (encode-symbol s l)))
              ((contains? s r) (cons 1 (encode-symbol s r)))
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

(define (insert t l)
  (cond ((null? l) (list t))
        ((< (weight t) (weight (car l))) (cons t l))
        (else (cons (car l) (insert t (cdr l))))))

(define (symbols t)
  (if (leaf? t)
      (list (leaf-symbol t))
      (branch-symbols t)))

(define (weight t)
  (if (leaf? t)
      (leaf-weight t)
      (branch-weight t)))

(define (counts l)
  (for/list ((s (in-list (sort (remove-duplicates l) symbol<?))))
    (let ((p (lambda (x) (eq? s x))))
      (cons s (length (filter p l))))))

(define song
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))
