#lang sicp

(define (make-table same-key?)
  (let* ((table (list '<table>))
         (assoc (lambda (key records)
                  (letrec ((a (lambda (records)
                                (cond ((null? records) #f)
                                      ((same-key? key (caar records)) (car records))
                                      (else (a (cdr records)))))))
                    (a records))))
         (lookup (lambda (list-of-keys)
                   (letrec ((lo (lambda (list-of-keys current-table)
                                  (if (null? list-of-keys)
                                      #f
                                      (let ((result (assoc (car list-of-keys) (cdr current-table))))
                                        (if result
                                            (if (null? (cdr list-of-keys))
                                                (cdr result)
                                                (lo (cdr list-of-keys) result))
                                            #f))))))
                     (lo list-of-keys table))))
         (insert! (lambda (list-of-keys value)
                    (letrec ((in (lambda (list-of-keys current-table)
                                   (if (null? list-of-keys)
                                       (error "insert!: empty list of keys")
                                       (let ((result (assoc (car list-of-keys) (cdr current-table))))
                                         (if result
                                             (if (null? (cdr list-of-keys))
                                                 (set-cdr! result value)
                                                 (in (car list-of-keys) result))
                                             (if (null? (cdr list-of-keys))
                                                 (set-cdr! current-table
                                                           (cons (cons (car list-of-keys)
                                                                       value)
                                                                 (cdr current-table)))
                                                 (let ((new-table (list (car list-of-keys))))
                                                   (set-cdr! current-table
                                                             (cons new-table
                                                                   (cdr current-table)))
                                                   (in (car list-of-keys) new-table)))))))))
                      (in list-of-keys table))))
         (dispatch (lambda (m)
                     (cond ((eq? m 'lookup) lookup)
                           ((eq? m 'insert!) insert!)
                           (else (error "make-table: undefined operation:" m))))))
    dispatch))

(define table (make-table equal?))

(define get (table 'lookup))

(define put (table 'insert!))
