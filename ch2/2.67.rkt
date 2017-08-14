#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define symbol-leaf cadr)

(define weight-leaf caddr)

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))

(define left-branch car)

(define right-branch cadr)

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (letrec ((choose-branch (lambda (bit branch)
                            (cond ((zero? bit) (left-branch branch))
                                  ((= bit 1) (right-branch branch))
                                  (else (error "decode: bad bit")))))
           (d (lambda (bits current-branch)
                (if (null? bits)
                    '()
                    (let ((next-branch
                           (choose-branch (car bits) current-branch)))
                      (if (leaf? next-branch)
                          (cons (symbol-leaf next-branch)
                                (d (cdr bits) tree))
                          (d (cdr bits) next-branch)))))))
    (d bits tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x)
            (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(let ((sample-tree (make-code-tree
                    (make-leaf 'A 4)
                    (make-code-tree
                     (make-leaf 'B 2)
                     (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
      (sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0)))
  (display (decode sample-message sample-tree)))
;; (A D A B B C A)