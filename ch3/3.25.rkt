#lang sicp

(define (make-table same-key?)
  (let* ((table (list '<table>))
         (get (lambda (ks)
                   (letrec ((g (lambda (ks current-table)
                                  (if (null? ks)
                                      #f
                                      (let ((result (assoc (car ks)
                                                           (cdr current-table))))
                                        (if result
                                            (if (null? (cdr ks))
                                                (cdr result)
                                                (g (cdr ks)
                                                    result))
                                            #f))))))
                     (g ks table))))
         (put (lambda (ks v)
                    (letrec ((p (lambda (ks current-table)
                                   (if (null? ks)
                                       (error "put: empty list of keys")
                                       (let ((result (assoc (car ks)
                                                            (cdr current-table))))
                                         (if result
                                             (if (null? (cdr ks))
                                                 (set-cdr! result v)
                                                 (p (car ks)
                                                     result))
                                             (if (null? (cdr ks))
                                                 (set-cdr! current-table
                                                           (cons (cons (car ks)
                                                                       v)
                                                                 (cdr current-table)))
                                                 (let ((new-table (list (car ks))))
                                                   (set-cdr! current-table
                                                             (cons new-table
                                                                   (cdr current-table)))
                                                   (p (car ks)
                                                       new-table)))))))))
                      (p ks table))))
         (dispatch (lambda (m)
                     (case m
                       ('get get)
                       ('put put)
                       (else (error "table: unknown method:" m))))))
    dispatch))

(define table (make-table equal?))

(define get (table 'get))

(define put (table 'put))
