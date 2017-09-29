#lang sicp

(#%require (only racket/base
                 make-hash
                 hash-ref
                 hash-set!))

(define (make-table)
  (let* ((table (make-hash))
         (get (lambda (key1 key2)
                (hash-ref table
                          (list key1 key2)
                          #f)))
         (put (lambda (key1 key2 value)
                (hash-set! table
                           (list key1 key2)
                           value)))
         (dispatch (lambda (m)
                     (case m
                       ('get get)
                       ('put put)
                       (else (error "table: unknown method:" m))))))
    dispatch))

(define table (make-table))

(define get (table 'get))

(define put (table 'put))

(define attach cons)

(define (type obj)
  (if (pair? obj)
      (car obj)
      (error "type: invalid object:" obj)))

(define (value obj)
  (if (pair? obj)
      (cdr obj)
      (error "value: invalid object:" obj)))

(define (apply-generic op . args)
  (let* ((types (map type args))
         (proc (get op types)))
    (if proc
        (apply proc (map value args))
        (error "apply-generic: no method for these types:" (list op types)))))

(define (get-record dept employee)
  (let ((r ((get 'record dept) employee)))
    (if (value r)
        (list dept
              employee
              (apply-generic 'address r)
              (apply-generic 'salary r))
        #f)))

(define department car)

(define name cadr)

(define address caddr)

(define salary cadddr)

(define (get-salary dept employee)
  (let ((r (get-record dept employee)))
    (if r
        (salary r)
        #f)))

(define (find-employee-record employee)
  (let ((depts '(dept-a dept-b)))
    (letrec ((f (lambda (depts)
                  (and (not (null? depts))
                       (or (get-record (car depts)
                                       employee)
                           (f (cdr depts)))))))
      (f depts))))

(define (displayln x)
  (display x)
  (newline))

(define (load-dept-a-records)
  (let ((records '(("Harold" "32 Vassar St" 10000)
                   ("Gerald" "32 Vassar St" 20000)))
        (tag (lambda (x)
               (attach 'dept-a x))))
    (display "loading department A records...")
    (put 'record 'dept-a
         (lambda (employee)
           (tag (assoc employee records))))
    (put 'address '(dept-a) cadr)
    (put 'salary '(dept-a) caddr)
    (displayln "done.")))

(define (load-dept-b-records)
  (let ((records '(("Ken" 30000 "600 Mountain Ave")
                   ("Dennis" 40000 "600 Mountain Ave")))
        (tag (lambda (x)
               (attach 'dept-b x))))
    (display "loading department B records...")
    (put 'record 'dept-b
         (lambda (employee)
           (tag (assoc employee records))))
    (put 'salary '(dept-b) cadr)
    (put 'address '(dept-b) caddr)
    (displayln "done.")))

(load-dept-a-records)
;; loading department A records...done.

(load-dept-b-records)
;; loading department B records...done.

(for-each (lambda (employee)
            (let ((r (find-employee-record employee)))
              (if r
                  (displayln (get-salary (department r)
                                         employee)))))
          (list "Harold"
                "Gerald"
                "Ken"
                "Dennis"))
;; 10000
;; 20000
;; 30000
;; 40000

;; When Insatiable takes over a new company, their information systems must tag
;; provided records with a unique identifier for that department, and provide
;; public, department-specific accessors for information within each record.
