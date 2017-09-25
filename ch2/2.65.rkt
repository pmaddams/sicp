#lang sicp

(define entry car)

(define left-branch cadr)

(define right-branch caddr)

(define (make-node entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (letrec ((e (lambda (set)
                (cond ((null? set)
                       #f)
                      ((< x (entry set))
                       (e (left-branch set)))
                      ((> x (entry set))
                       (e (right-branch set)))
                      (else
                       #t)))))
    (e set)))

(define (adjoin-set x set)
  (letrec ((a (lambda (set)
                (cond ((null? set)
                       (make-node x '() '()))
                      ((< x (entry set))
                       (make-node (entry set)
                                  (a (left-branch set))
                                  (right-branch set)))
                      ((> x (entry set))
                       (make-node (entry set)
                                  (left-branch set)
                                  (a (right-branch set))))
                      (else set)))))
    (a set)))

(define (tree->list t)
  (letrec ((tl (lambda (t result)
                 (if (null? t)
                     result
                     (tl (left-branch t)
                         (cons (entry t)
                               (tl (right-branch t)
                                   result)))))))
    (tl t '())))

(define (list->tree l)
  (letrec ((lt (lambda (l n)
                 (if (zero? n)
                     (cons '() l)
                     (let* ((left-size (quotient (dec n) 2))
                            (right-size (- n (inc left-size)))
                            (left-result (lt l
                                             left-size))
                            (left-tree (car left-result))
                            (non-left-l (cdr left-result))
                            (this-entry (car non-left-l))
                            (right-result (lt (cdr non-left-l)
                                              right-size))
                            (right-tree (car right-result))
                            (remaining-l (cdr right-result)))
                       (cons (make-node this-entry
                                        left-tree
                                        right-tree)
                             remaining-l))))))
    (car (lt l (length l)))))

(define (intersection-set set1 set2)
  (letrec ((i (lambda (l result)
                (cond ((null? l)
                       (list->tree result))
                      ((element-of-set? (car l) set2)
                       (i (cdr l)
                          (append result
                                  (list (car l)))))
                      (else
                       (i (cdr l)
                          result))))))
    (i (tree->list set1) '())))

(define (union-set set1 set2)
  (letrec ((u (lambda (l result)
                (cond ((null? l)
                       (list->tree (append result
                                           (tree->list set2))))
                      ((element-of-set? (car l) set2)
                       (u (cdr l)
                          result))
                      (else
                       (u (cdr l)
                          (append result
                                  (list (car l)))))))))
    (u (tree->list set1) '())))

(define (displayln x)
  (display x)
  (newline))

(let ((set1 (list->tree '(1 2 3 4 5)))
      (set2 (list->tree '(3 4 5 6 7))))
  (for-each (lambda (t)
              (displayln (tree->list t)))
            (list (intersection-set set1 set2)
                  (union-set set1 set2)
                  (intersection-set set1 (union-set set1 set2)))))
;; (3 4 5)
;; (1 2 3 4 5 6 7)
;; (1 2 3 4 5)
