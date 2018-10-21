#lang sicp

(define (square x)
  (expt x 2))

(define (square-tree-a t)
  (cond ((null? t) '())
        ((not (pair? t)) (square t))
        (else (cons (square-tree-a (car t))
                    (square-tree-a (cdr t))))))

(define (square-tree-b t)
  (map (lambda (st)
         (if (not (pair? st))
             (square st)
             (square-tree-b st)))
       t))

(define (displayln x)
  (display x)
  (newline))

(let ((t '(1 (2 (3 4) 5) (6 7))))
  (displayln (square-tree-a t))
  (displayln (square-tree-b t)))
;; (1 (4 (9 16) 25) (36 49))
;; (1 (4 (9 16) 25) (36 49))
