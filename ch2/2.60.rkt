#lang sicp

(define (element-of-set? x set)
  (letrec ((e (lambda (set)
                (cond ((null? set) #f)
                      ((equal? x (car set)) #t)
                      (else (e (cdr set)))))))
    (e set)))

(define adjoin-set cons)

(define (intersection-set set1 set2)
  (letrec ((i (lambda (set1)
                (cond ((null? set1) '())
                      ((element-of-set? (car set1) set2)
                       (cons (car set1) (i (cdr set1))))
                      (else (i (cdr set1)))))))
    (i set1)))

(define union-set append)

(define (displayln x)
  (display x)
  (newline))

(let ((set1 '(a b c d e))
      (set2 '(c d e f g)))
  (displayln (intersection-set set1 set2))
  (displayln (union-set set1 set2))
  (displayln (intersection-set set1 (union-set set1 set2))))
;; (c d e)
;; (a b c d e c d e f g)
;; (a b c d e)

;; Using a multiset representation of sets greatly simplifies the adjoin-set and
;; union-set operations, which are now equivalent to cons and append,
;; respectively. The element-of-set? and intersection-set operations are
;; unchanged. This representation might be preferred if a large number of
;; distinct or mostly distinct sets needed to be concatenated, followed by a
;; single intersection.