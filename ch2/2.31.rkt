#lang sicp

(define (tree-map proc t)
  (letrec ((tm (lambda (st)
                 (if (not (pair? st))
                     (proc st)
                     (map tm st)))))
    (tm t)))

(define (square x)
  (expt x 2))

(let ((t '(1 (2 (3 4) 5) (6 7))))
  (display (tree-map square t)))
;; (1 (4 (9 16) 25) (36 49))
