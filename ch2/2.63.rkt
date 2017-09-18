#lang sicp

(define entry car)

(define left-branch cadr)

(define right-branch caddr)

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (letrec ((e (lambda (set)
                (cond ((null? set) #f)
                      ((< x (entry set)) (e (left-branch set)))
                      ((> x (entry set)) (e (right-branch set)))
                      (else #t)))))
    (e set)))

(define (adjoin-set x set)
  (letrec ((a (lambda (set)
                (cond ((null? set)
                       (make-tree x '() '()))
                      ((< x (entry set))
                       (make-tree (entry set)
                                  (a (left-branch set))
                                  (right-branch set)))
                      ((> x (entry set))
                       (make-tree (entry set)
                                  (left-branch set)
                                  (a (right-branch set))))
                      (else set)))))
    (a set)))

(define (tree->list-a t)
  (if (null? t)
      '()
      (append (tree->list-a (left-branch t))
              (cons (entry t)
                    (tree->list-a (right-branch t))))))

(define (tree->list-b t)
  (letrec ((tl (lambda (t result)
                 (if (null? t)
                     result
                     (tl (left-branch t)
                         (cons (entry t)
                               (tl (right-branch t)
                                   result)))))))
    (tl t '())))

(define (displayln x)
  (display x)
  (newline))

(let ((t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
      (t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
      (t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))))
  (for-each displayln
            (list (tree->list-a t1)
                  (tree->list-b t1)
                  (tree->list-a t2)
                  (tree->list-b t2)
                  (tree->list-a t3)
                  (tree->list-b t3))))
;; (1 3 5 7 9 11)
;; (1 3 5 7 9 11)
;; (1 3 5 7 9 11)
;; (1 3 5 7 9 11)
;; (1 3 5 7 9 11)
;; (1 3 5 7 9 11)

;; The first procedure is tree recursive, so the order of growth in number of
;; steps is exponential with respect to the depth of the tree. Since the depth
;; of a balanced tree with n elements is logarithmic with respect to n, the
;; first procedure has overall growth proportional to n, i.e. linear. The second
;; procedure uses tail recursion, so it evolves iteratively. This means the
;; order of growth in number of steps is proportional to the depth of the tree,
;; and the overall order of growth is logarithmic with respect to n for a
;; balanced tree.
