#lang sicp

(define record car)

(define left-branch cadr)

(define right-branch caddr)

(define (make-record given-key given-value)
  (list given-key given-value))

(define key car)

(define value cadr)

(define (make-tree given-record left right)
  (list given-record left right))

(define (lookup given-key set-of-records)
  (letrec ((l (lambda (set-of-records)
                (if (null? set-of-records)
                    #f
                    (let* ((this-record (record set-of-records))
                           (this-key (key this-record)))
                      (cond ((< given-key this-key)
                             (l (left-branch set-of-records)))
                            ((> given-key this-key)
                             (l (right-branch set-of-records)))
                            (else
                             this-record)))))))
    (l set-of-records)))

(define (insert given-record set-of-records)
  (letrec ((given-key (key given-record))
           (i (lambda (set-of-records)
                (if (null? set-of-records)
                    (make-tree given-record '() '())
                    (let* ((this-record (record set-of-records))
                           (this-key (key this-record)))
                      (cond ((< given-key this-key)
                             (make-tree this-record
                                        (i (left-branch set-of-records))
                                        (right-branch set-of-records)))
                            ((> given-key this-key)
                             (make-tree this-record
                                        (left-branch set-of-records)
                                        (i (right-branch set-of-records))))
                            (else
                             set-of-records)))))))
    (i set-of-records)))

(define (make-records . args)
  (letrec ((m (lambda (args result)
                (if (null? args)
                    result
                    (m (cdr args)
                       (insert (car args)
                               result))))))
    (m args '())))

(define (displayln x)
  (display x)
  (newline))

(define (enumerate-interval low high)
  (letrec ((e (lambda (i result)
                (if (< i low)
                    result
                    (e (dec i) (cons i result))))))
    (e high '())))

(let ((set-of-records (make-records '(1 "foo")
                                    '(2 "bar")
                                    '(3 "baz")
                                    '(4 "qux")
                                    '(5 "quux"))))
  (for-each (lambda (key)
              (displayln (value (lookup key set-of-records))))
            (enumerate-interval 1 5)))
;; foo
;; bar
;; baz
;; qux
;; quux
