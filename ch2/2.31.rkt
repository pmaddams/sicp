#lang sicp

(define (tree-map p t)
  (letrec ((pt (lambda (st)
                 (if (not (pair? st))
                     (p st)
                     (map pt st)))))
    (pt t)))

(let ((square (lambda (x)
                (expt x 2)))
      (t '(1 (2 (3 4) 5) (6 7))))
  (display (tree-map square t)))
;; (1 (4 (9 16) 25) (36 49))