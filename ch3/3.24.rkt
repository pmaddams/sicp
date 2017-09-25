#lang sicp

(define (make-table same?)
  (let* ((table (list '<table>))
         (assm (lambda (key records)
                 (letrec ((a (lambda (records)
                               (cond ((null? records)
                                      #f)
                                     ((same? key (caar records))
                                      (car records))
                                     (else
                                      (a (cdr records)))))))
                   (a records))))
         (get (lambda (key)
                (let ((record (assm key (cdr table))))
                  (if record
                      (cdr record)
                      #f))))
         (put (lambda (key value)
                (let ((record (assm key (cdr table))))
                  (if record
                      (set-cdr! record value)
                      (set-cdr! table
                                (cons (cons key value)
                                      (cdr table)))))))
         (dispatch (lambda (m)
                     (case m
                       ('get get)
                       ('put put)
                       (else "table: unknown method:" m)))))
    dispatch))

(define table (make-table eq?))

(define get (table 'get))

(define put (table 'put))

(put 'a 'apple)
(put 'b 'banana)
(get 'a)
(get 'b)
(get 'c)
;; 'apple
;; 'banana
;; #f
