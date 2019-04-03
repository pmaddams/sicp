#lang racket/base

; Exercise 2.70

(provide (all-defined-out))

(require racket/list)

(struct leaf (symbol weight))

(struct branch (symbols weight left right))

(define (make-huffman-tree symbols)
  (make-branches (make-leaves (counts symbols))))

(define (make-branches leaves)
  (unless (> (length leaves) 1)
    (error "not enough information:" leaves))
  (let loop ((leaves (cddr leaves))
             (acc (make-branch (car leaves) (cadr leaves))))
    (if (null? leaves)
        acc
        (loop (cdr leaves)
              (make-branch (car leaves) acc)))))

(define (make-branch left right)
  (branch (append (symbols left) (symbols right))
          (+ (weight left) (weight right))
          left
          right))

(define (make-leaves pairs)
  (define (insert leaf set)
    (cond ((null? set) (list leaf))
          ((< (weight leaf) (weight (car set))) (cons leaf set))
          (else (cons (car set) (insert leaf (cdr set))))))

  (define (make-leaf pair)
    (leaf (car pair) (cdr pair)))

  (foldr insert '() (map make-leaf pairs)))

(define (encode symbols t)
  (append-map (lambda (s) (encode-symbol s t)) symbols))

(define (encode-symbol s t)
  (let loop ((t t) (acc '()))
    (cond ((leaf? t) (reverse acc))
          ((contains? s (branch-left t)) (loop (branch-left t) (cons 0 acc)))
          ((contains? s (branch-right t)) (loop (branch-right t) (cons 1 acc)))
          (else (error "unknown symbol:" s)))))

(define (contains? s t)
  (member s (symbols t)))

(define (decode bits root)
  (let loop ((bits bits) (current root))
    (if (null? bits)
        '()
        (let ((next (choose-branch (car bits) current)))
          (if (leaf? next)
              (cons (leaf-symbol next) (loop (cdr bits) root))
              (loop (cdr bits) next))))))

(define (choose-branch bit t)
  (if (zero? bit)
      (branch-left t)
      (branch-right t)))

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
