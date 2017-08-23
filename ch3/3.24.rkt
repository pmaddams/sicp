#lang sicp

(define (make-table same-key?)
  (let* ((table (list '<table>))
         (assoc (lambda (key records)
                  (letrec ((a (lambda (records)
                                (cond ((null? records) #f)
                                      ((same-key? key (caar records)) (car records))
                                      (else (a (cdr records)))))))
                    (a records))))
         (lookup (lambda (key1 key2)
                   (let ((subtable (assoc key1 (cdr table))))
                     (if subtable
                         (let ((record (assoc key2 (cdr subtable))))
                           (if record
                               (cdr record)
                               #f))
                         #f))))
         (insert! (lambda (key1 key2 value)
                   (let ((subtable (assoc key1 (cdr table))))
                     (if subtable
                         (let ((record (assoc key2 (cdr subtable))))
                           (if record
                               (set-cdr! record value)
                               (set-cdr! subtable
                                         (cons (cons key2 value)
                                               (cdr subtable)))))
                         (set-cdr! table
                                   (cons (list key1
                                               (cons key2 value))
                                         (cdr table)))))))
         (dispatch (lambda (m)
                     (cond ((eq? m 'lookup) lookup)
                           ((eq? m 'insert!) insert!)
                           (else (error "make-table: undefined operation:" m))))))
    dispatch))

(define table (make-table equal?))

(define get (table 'lookup))

(define put (table 'insert!))

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