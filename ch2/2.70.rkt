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
                  (and (not (null? current-symbols))
                       (or (eq? symbol (car current-symbols))
                           (s (cdr current-symbols)))))))
      (s current-symbols))))

(define (encode-symbol symbol tree)
  (letrec ((e (lambda (tree result)
                (cond ((leaf? tree)
                       result)
                      ((symbol-in-tree? symbol (right-branch tree))
                       (e (right-branch tree)
                          (append result
                                  (list 1))))
                      ((symbol-in-tree? symbol (left-branch tree))
                       (e (left-branch tree)
                          (append result
                                  (list 0))))
                      (else (error "encode: unknown symbol:" symbol))))))
    (e tree '())))

(define (encode message tree)
  (letrec ((e (lambda (message)
                (if (null? message)
                    '()
                    (append (encode-symbol (car message) tree)
                            (encode (cdr message) tree))))))
    (e message)))

(define (decode bits tree)
  (let ((choose-branch (lambda (bit branch)
                         (case bit
                           (0 (left-branch branch))
                           (1 (right-branch branch))
                           (else (error "decode: unknown bit:" bit))))))
    (letrec ((d (lambda (bits this-branch)
                  (if (null? bits)
                      '()
                      (let ((next-branch (choose-branch (car bits)
                                                        this-branch)))
                        (if (leaf? next-branch)
                            (cons (symbol-leaf next-branch)
                                  (d (cdr bits)
                                     tree))
                            (d (cdr bits)
                               next-branch)))))))
      (d bits tree))))

(define (adjoin-set x set)
  (cond ((null? set)
         (list x))
        ((< (weight x)
            (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (successive-merge leaf-set)
  (letrec ((s (lambda (l result)
                (if (null? l)
                    result
                    (s (cdr l)
                       (make-code-tree (car l)
                                       result))))))
    (s (cddr leaf-set)
       (make-code-tree (cadr leaf-set)
                       (car leaf-set)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(let ((alphabet (generate-huffman-tree '((A 2)
                                         (BOOM 1)
                                         (GET 2)
                                         (JOB 2)
                                         (NA 16)
                                         (SHA 3)
                                         (YIP 9)
                                         (WAH 1))))
      (song '(GET A JOB
                  SHA NA NA NA NA NA NA NA NA
                  GET A JOB
                  SHA NA NA NA NA NA NA NA NA
                  WAH YIP YIP YIP YIP YIP YIP YIP YIP
                  SHA BOOM)))
  (display "song length: ")
  (display (length song))
  (newline)
  (display "number of bits: ")
  (display (length (encode song alphabet)))
  (newline))
;; song length: 35
;; number of bits: 85

;; For an 8-letter alphabet, only 3 bits are required to encode every symbol in
;; a fixed-length code. The number of bits is therefore 3 times the number of
;; symbols in the message, or 3 * 35 = 105.
