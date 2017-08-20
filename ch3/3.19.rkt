#lang sicp

(define (has-cycle? l)
  (letrec ((next (lambda (l)
                   (if (null? l)
                       '()
                       (cdr l))))
           (h (lambda (l1 l2)
                (cond ((or (null? l1)
                           (null? l2)) #f)
                      ((or (eq? l1 l2)
                           (eq? l1 (next l2))) #t)
                      (else (h (next l1) (next (next l2))))))))
    (h l (next l))))

(define (displayln x)
  (display x)
  (newline))

(let* ((a '(3))
       (b (cons 2 a))
       (c (cons 1 b)))
  (set-cdr! a b)
  (for-each (lambda (l)
              (displayln l)
              (displayln (has-cycle? l)))
            (list a b c)))
;; #0=(3 2 . #0#)
;; #t
;; #0=(2 3 . #0#)
;; #t
;; (1 . #0=(2 3 . #0#))
;; #t
