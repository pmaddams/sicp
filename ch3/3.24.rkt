#lang sicp

(define (make-table same-key?)
  (let* ((table (list '<table>))
         (get (lambda (k1 k2)
                   (let ((subtable (assoc k1 (cdr table))))
                     (if subtable
                         (let ((record (assoc k2 (cdr subtable))))
                           (if record
                               (cdr record)
                               #f))
                         #f))))
         (put (lambda (k1 k2 v)
                    (let ((subtable (assoc k1 (cdr table))))
                      (if subtable
                          (let ((record (assoc k2 (cdr subtable))))
                            (if record
                                (set-cdr! record v)
                                (set-cdr! subtable
                                          (cons (cons k2 v)
                                                (cdr subtable)))))
                          (set-cdr! table
                                    (cons (list k1
                                                (cons k2 v))
                                          (cdr table)))))))
         (dispatch (lambda (m)
                     (case m
                       ('get get)
                       ('put put)
                       (else (error "table: unknown method:" m))))))
    dispatch))

(define table (make-table equal?))

(define get (table 'get))

(define put (table 'put))

(put 'a 'a 'apple)
(put 'a 'b 'banana)
(put 'b 'a 'orange)
(get 'a 'a)
(get 'a 'b)
(get 'b 'a)
(get 'b 'b)
;; 'apple
;; 'banana
;; 'orange
;; #f
