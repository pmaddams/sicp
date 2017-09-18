#lang sicp

(define (element-of-set? x set)
  (letrec ((e (lambda (set)
                (and (not (null? set))
                     (or (= x (car set))
                         (and (> x (car set))
                              (e (cdr set))))))))
    (e set)))

(define (adjoin-set x set)
  (letrec ((a (lambda (set)
                (cond ((or (null? set)
                           (< x (car set)))
                       (cons x set))
                      ((= x (car set))
                       set)
                      (else
                       (cons (car set)
                             (a (cdr set))))))))
    (a set)))

(define (intersection-set set1 set2)
  (if (or (null? set1)
          (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((< x1 x2)
               (intersection-set (cdr set1)
                                 set2))
              ((> x1 x2)
               (intersection-set set1
                                 (cdr set2)))
              (else
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((< x1 x2)
                       (cons x1
                             (union-set (cdr set1)
                                        set2)))
                      ((> x1 x2)
                       (cons x2
                             (union-set set1
                                        (cdr set2))))
                      (else
                       (cons x1
                             (union-set (cdr set1)
                                        (cdr set2)))))))))

(define (displayln x)
  (display x)
  (newline))

(let ((set1 '(1 2 3 4 5))
      (set2 '(3 4 5 6 7)))
  (displayln (intersection-set set1 set2))
  (displayln (union-set set1 set2)))
;; (3 4 5)
;; (1 2 3 4 5 6 7)
