#lang sicp

(define (memq item x)
  (letrec ((m (lambda (x)
                (cond ((null? x) #f)
                      ((eq? item (car x)) x)
                      (else (m (cdr x)))))))
    (m x)))

(define (displayln x)
  (display x)
  (newline))

(displayln (list 'a 'b 'c))
;; (a b c)

(displayln (list (list 'george)))
;; ((george))

(displayln (cdr '((x1 x2) (y1 y2))))
;; ((y1 y2))

(displayln (cadr '((x1 x2) (y1 y2))))
;; (y1 y2)

(displayln (pair? (car '(a short list))))
;; #f

(displayln (memq 'red '((red shoes) (blue socks))))
;; #f

(displayln (memq 'red '(red shoes blue socks)))
;; (red shoes blue socks)