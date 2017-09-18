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

(define (displayln x)
  (display x)
  (newline))

(let ((set '(1 3 5)))
  (set! set (adjoin-set 2 set))
  (displayln set)
  (set! set (adjoin-set 3 set))
  (displayln set)
  (set! set (adjoin-set 4 set))
  (displayln set))
;; (1 2 3 5)
;; (1 2 3 5)
;; (1 2 3 4 5)
