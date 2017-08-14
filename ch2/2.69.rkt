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

(define (symbol-in-tree? symbol tree)
  (let ((current-symbols (symbols tree)))
    (letrec ((s (lambda (current-symbols)
                  (cond ((null? current-symbols) #f)
                        ((eq? symbol (car current-symbols)) #t)
                        (else (s (cdr current-symbols)))))))
      (s current-symbols))))

(define (encode-symbol symbol tree)
  (letrec ((e (lambda (tree result)
                (cond ((leaf? tree) result)
                      ((symbol-in-tree? symbol (left-branch tree))
                       (e (left-branch tree) (append result (list 0))))
                      ((symbol-in-tree? symbol (right-branch tree))
                       (e (right-branch tree) (append result (list 1))))
                      (else (error "encode-symbol: not in tree" symbol))))))
    (e tree '())))

(define (encode message tree)
  (letrec ((e (lambda (message)
                (if (null? message)
                    '()
                    (append (encode-symbol (car message) tree)
                            (encode (cdr message) tree))))))
    (e message)))

(define (decode bits tree)
  (letrec ((choose-branch (lambda (bit branch)
                            (cond ((zero? bit) (left-branch branch))
                                  ((= bit 1) (right-branch branch))
                                  (else (error "choose-branch: bad branch" bit)))))
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

(define (successive-merge leaf-set)
  (let* ((first-right (car leaf-set))
         (first-left (cadr leaf-set))
         (first-tree (make-code-tree first-left first-right)))
    (letrec ((s (lambda (remaining-leaves tree-so-far)
                  (if (null? remaining-leaves)
                      tree-so-far
                      (s (cdr remaining-leaves)
                         (make-code-tree (car remaining-leaves)
                                         tree-so-far))))))
      (s (cddr leaf-set) first-tree))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (displayln x)
  (display x)
  (newline))

(let ((sample-tree (make-code-tree
                    (make-leaf 'A 4)
                    (make-code-tree
                     (make-leaf 'B 2)
                     (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
      (pairs '((A 4) (B 2) (D 1) (C 1))))
  (displayln sample-tree)
  (displayln (generate-huffman-tree pairs)))
;; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
;; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)