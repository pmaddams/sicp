#lang sicp

(define (make-node record left right)
  (list record left right))

(define record-of car)

(define left-branch cadr)

(define right-branch caddr)

(define make-record cons)

(define key-of car)

(define value-of cdr)

(define (lookup key tree)
  (letrec ((l (lambda (tree)
                (if (null? tree)
                    #f
                    (let* ((this-record (record-of tree))
                           (this-key (key-of this-record)))
                      (cond ((< key this-key)
                             (l (left-branch tree)))
                            ((> key this-key)
                             (l (right-branch tree)))
                            (else
                             (value-of this-record))))))))
    (l tree)))

(define (insert key value tree)
  (let ((record (make-record key value)))
    (letrec ((i (lambda (tree)
                  (if (null? tree)
                      (make-node record '() '())
                      (let* ((this-record (record-of tree))
                             (this-key (key-of this-record)))
                        (cond ((< key this-key)
                               (make-node this-record
                                          (i (left-branch tree))
                                          (right-branch tree)))
                              ((> key this-key)
                               (make-node this-record
                                          (left-branch tree)
                                          (i (right-branch tree))))
                              (else
                               tree)))))))
      (i tree))))

(define (make-tree l)
  (letrec ((m (lambda (l result)
                (if (null? l)
                    result
                    (m (cdr l)
                       (let* ((record (car l))
                              (key (key-of record))
                              (value (value-of record)))
                         (insert key value result)))))))
    (m l '())))

(define (displayln x)
  (display x)
  (newline))

(define (enumerate-interval low high)
  (letrec ((e (lambda (i result)
                (if (< i low)
                    result
                    (e (dec i)
                       (cons i result))))))
    (e high '())))

(let ((tree (make-tree '((1 . "foo")
                         (2 . "bar")
                         (3 . "baz")
                         (4 . "qux")
                         (5 . "quux")))))
  (for-each (lambda (key)
              (displayln (lookup key tree)))
            (enumerate-interval 1 5)))
;; foo
;; bar
;; baz
;; qux
;; quux
