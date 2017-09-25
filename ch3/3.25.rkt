#lang sicp

(define (make-table same?)
  (let* ((table '((<table>)))
         (assm (lambda (key records)
                 (letrec ((a (lambda (records)
                               (cond ((null? records)
                                      #f)
                                     ((same? key (caaar records))
                                      (car records))
                                     (else
                                      (a (cdr records)))))))
                   (a records))))
         (get (lambda (keys)
                (letrec ((g (lambda (keys table)
                              (if (null? keys)
                                  #f
                                  (let ((found (assm (car keys)
                                                     (cdr table))))
                                    (if found
                                        (if (null? (cdr keys))
                                            (cdar found)
                                            (g (cdr keys)
                                               found))
                                        #f))))))
                  (g keys table))))
         (put (lambda (keys value)
                (letrec ((p (lambda (keys table)
                              (let ((found (assm (car keys)
                                                 (cdr table))))
                                (if found
                                    (if (null? (cdr keys))
                                        (set-cdr! (car found) value)
                                        (p (cdr keys)
                                           found))
                                    (if (null? (cdr keys))
                                        (set-cdr! table
                                                  (cons (list (cons (car keys)
                                                                    value))
                                                        (cdr table)))
                                        (let ((new (list (list (car keys)))))
                                          (set-cdr! table
                                                    (cons new
                                                          (cdr table)))
                                          (p (cdr keys)
                                             new))))))))
                  (p keys table))))
         (dispatch (lambda (m)
                     (case m
                       ('get get)
                       ('put put)
                       (else (error "table: unknown method:" m))))))
    dispatch))

(define table (make-table eq?))

(define get (table 'get))

(define put (table 'put))

(put '(a) 'apple)
(put '(a b) 'banana)
(get '(a))
(get '(a b))
(get '(a b c))
;; 'apple
;; 'banana
;; #f
