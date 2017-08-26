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

(define (make-personnel-file-a)
  (let* ((all-records '())
         (record car)
         (left-branch cadr)
         (right-branch caddr)
         (make-record (lambda (given-employee given-address given-salary)
                        (list given-employee given-address given-salary)))
         (employee car)
         (address cadr)
         (salary caddr)
         (make-tree (lambda (given-record left right)
                      (list given-record left right)))
         (get-record
          (lambda (given-employee-sym)
            (letrec ((given-employee (symbol->string given-employee-sym))
                     (l (lambda (set-of-records)
                          (if (null? set-of-records)
                              #f
                              (let* ((this-record (record set-of-records))
                                     (this-employee (symbol->string
                                                     (employee this-record))))
                                (cond ((string<? given-employee this-employee)
                                       (l (left-branch set-of-records)))
                                      ((string>? given-employee this-employee)
                                       (l (right-branch set-of-records)))
                                      ((string=? given-employee this-employee)
                                       this-record)
                                      (else
                                       #f)))))))
              (l all-records))))
         (add-record
          (lambda (given-record)
            (letrec ((given-employee (symbol->string (employee given-record)))
                     (a (lambda (set-of-records)
                          (if (null? set-of-records)
                              (make-tree given-record '() '())
                              (let ((this-employee (symbol->string
                                                    (employee (record set-of-records)))))
                                (cond ((string<? given-employee this-employee)
                                       (make-tree (record set-of-records)
                                                  (a (left-branch set-of-records))
                                                  (right-branch set-of-records)))
                                      ((string>? given-employee this-employee)
                                       (make-tree (record set-of-records)
                                                  (left-branch set-of-records)
                                                  (a (right-branch set-of-records))))
                                      (else set-of-records)))))))
              (set! all-records (a all-records)))))
         (dispatch (lambda (m)
                     (cond ((eq? m 'add-record)
                            (lambda (given-employee given-address given-salary)
                              (add-record
                               (make-record given-employee given-address given-salary))))
                           ((eq? m 'get-record)
                            (lambda (given-employee)
                              (let ((this-record (get-record given-employee)))
                                (if (not this-record)
                                    #f
                                    (let* ((this-address (address this-record))
                                           (this-salary (salary this-record)))
                                      (list given-employee this-address this-salary))))))
                           (else
                            (error "personnel-file-a: undefined operation:" m))))))
    dispatch))

(define (make-personnel-file-b)
  (let* ((all-records '())
         (record car)
         (make-record (lambda (given-employee given-address given-salary)
                        (list given-employee given-address given-salary)))
         (employee car)
         (address cadr)
         (salary caddr)
         (get-record (lambda (given-employee-sym)
                       (letrec ((given-employee (symbol->string given-employee-sym))
                                (l (lambda (set-of-records)
                                     (if (null? set-of-records)
                                         #f
                                         (let* ((this-record (record set-of-records))
                                                (this-employee (symbol->string
                                                                (employee this-record))))
                                           (if (string=? given-employee this-employee)
                                               this-record
                                               (l (cdr set-of-records))))))))
                         (l all-records))))
         (add-record (lambda (given-record)
                       (set! all-records (cons given-record all-records))))
         (dispatch (lambda (m)
                     (cond ((eq? m 'add-record)
                            (lambda (given-employee given-address given-salary)
                              (add-record
                               (make-record given-employee given-address given-salary))))
                           ((eq? m 'get-record)
                            (lambda (given-employee)
                              (let ((this-record (get-record given-employee)))
                                (if (not this-record)
                                    #f
                                    (let* ((this-address (address this-record))
                                           (this-salary (salary this-record)))
                                      (list given-employee this-address this-salary))))))
                           (else (error "personnel-file-b: undefined operation:" m))))))
    dispatch))

(define personnel-file-a (make-personnel-file-a))

(define personnel-file-b (make-personnel-file-b))

(put 'personnel-file-a 'get-record (personnel-file-a 'get-record))

(put 'personnel-file-a 'add-record (personnel-file-a 'add-record))

(put 'personnel-file-b 'get-record (personnel-file-b 'get-record))

(put 'personnel-file-b 'add-record (personnel-file-b 'add-record))

(define (get-record personnel-file employee)
  ((get personnel-file 'get-record) employee))

(define (add-record personnel-file employee address salary)
  ((get personnel-file 'add-record) employee address salary))

(define (get-salary personnel-file employee)
  (caddr (get-record personnel-file employee)))

(define (find-employee-record employee)
  (let ((all-personnel-files (list
                              'personnel-file-a
                              'personnel-file-b)))
    (letrec ((f (lambda (personnel-files)
                  (if (null? personnel-files)
                      #f
                      (let* ((personnel-file (car personnel-files))
                             (employee-record (get-record personnel-file employee)))
                        (if employee-record
                            employee-record
                            (f (cdr personnel-files))))))))
      (f all-personnel-files))))

;; A uniform record API should be implemented that uses a layer of "glue code"
;; to present records in the same way to the interdepartmental system. This way,
;; the representation of records is irrelevant. When a new company is acquired,
;; this API service should be implemented on top of that company's existing
;; database.