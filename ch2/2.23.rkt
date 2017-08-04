#lang sicp

(define (foreach p l)
  (if (not (null? l))
      (begin (p (car l))
             (foreach p (cdr l)))))

(let ((p (lambda (x)
           (begin (display x)
                  (newline))))
      (l '(57 321 88)))
  (foreach p l))
;; 57
;; 321
;; 88