#lang sicp

(define (make-table)
  (let* ((tree '())
         (make-node (lambda (record)
                      (list record '() '())))
         (record-of car)
         (left-branch cadr)
         (right-branch caddr)
         (make-record cons)
         (key-of car)
         (value-of cdr)
         (get (lambda (key)
                (letrec ((g (lambda (tree)
                              (if (null? tree)
                                  #f
                                  (let* ((this-record (record-of tree))
                                         (this-key (key-of this-record)))
                                    (cond ((< key this-key)
                                           (g (left-branch tree)))
                                          ((> key this-key)
                                           (g (right-branch tree)))
                                          (else
                                           (value-of this-record))))))))
                  (g tree))))
         (put (lambda (key value)
                (let ((record (make-record key value)))
                  (letrec ((p (lambda (tree)
                                (let* ((this-record (record-of tree))
                                       (this-key (key-of this-record)))
                                  (cond ((< key this-key)
                                         (if (null? (left-branch tree))
                                             (set-car! (cdr tree)
                                                       (make-node record))
                                             (p (left-branch tree))))
                                        ((> key this-key)
                                         (if (null? (right-branch tree))
                                             (set-car! (cdr (cdr tree))
                                                       (make-node record))
                                             (p (right-branch tree))))
                                        (else
                                         (set-cdr! this-record
                                                   value)))))))
                    (if (null? tree)
                        (set! tree (make-node record))
                        (p tree))))))
         (dispatch (lambda (m)
                     (case m
                       ('get get)
                       ('put put)
                       (else (error "table: unknown method:" m))))))
    dispatch))

(define table (make-table))

(define get (table 'get))

(define put (table 'put))

(put 2 'apple)
(put 3 'banana)
(put 1 'orange)
(get 1)
(get 2)
(get 3)
(get 4)
;; 'orange
;; 'apple
;; 'banana
;; #f
