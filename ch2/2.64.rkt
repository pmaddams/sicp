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

(define (make-set . args)
  (letrec ((m (lambda (args result)
                (if (null? args)
                    result
                    (m (cdr args) (adjoin-set (car args) result))))))
    (m args '())))

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
                            (left-result (lt l left-size))
                            (left-tree (car left-result))
                            (non-left-l (cdr left-result))
                            (right-size (- n (inc left-size)))
                            (this-entry (car non-left-l))
                            (right-result (lt (cdr non-left-l) right-size))
                            (right-tree (car right-result))
                            (remaining-l (cdr right-result)))
                       (cons (make-tree this-entry left-tree right-tree)
                             remaining-l))))))
    (car (lt l (length l)))))

;; The partial-tree procedure, here locally defined as lt, has a recursive base
;; case where n, the number of elements in the subtree, is zero. We descend the
;; tree leftwards, floor dividing the number of elements by 2 repeatedly until n
;; becomes zero, to reach the correct depth for the final tree. Now we take the
;; cons of the empty list and the list of elements to be the left result, and
;; the car, which is now the empty list, to be the left tree. The second element
;; of the list becomes the node entry. The right subtree is then constructed
;; from the car of another call to partial-tree at the same depth. We generate a
;; node using make-tree, from the current node entry, the left subtree, and the
;; right subtree. This node is then consed to the result of the right subtree
;; from the node above. We continue walking up the tree until the list of
;; remaining elements is empty, at which point we have generated the entire
;; balanced tree structure.

(define (displayln x)
  (display x)
  (newline))

(let* ((t1 (make-set 1 3 5 7 9 11))
       (l (tree->list t1))
       (t2 (list->tree l)))
  (for-each displayln (list t1 l t2)))
;; (1 () (3 () (5 () (7 () (9 () (11 () ()))))))
;; (1 3 5 7 9 11)
;; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;;  5
;;  /\
;; 1  9
;; \  /\
;; 3 7 11

;; The process is tree recursive, which means it requires an exponential number
;; of steps with respect to the depth of the tree. However, since we divide the
;; problem in half at each step, the depth of the tree is logarithmic with
;; respect to the number of nodes. The overall order of growth is proportional
;; to e^(log n), or linear.