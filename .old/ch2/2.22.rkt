#lang sicp

(define (square x)
  (expt x 2))

(define (square-list-a l)
  (letrec ((s (lambda (l result)
                (if (null? l)
                    result
                    (s (cdr l)
                       (cons (square (car l))
                             result))))))
    (s l '())))

(define (displayln x)
  (display x)
  (newline))

(displayln (square-list-a '(1 2 3 4)))
;; (16 9 4 1)

;; This procedure conses each squared item in the list to the front of the list
;; so far, so the list ends up in reverse order.

(define (square-list-b l)
  (letrec ((s (lambda (l result)
                (if (null? l)
                    result
                    (s (cdr l)
                       (cons result
                             (square (car l))))))))
    (s l '())))

(displayln (square-list-b '(1 2 3 4)))
;; ((((() . 1) . 4) . 9) . 16)

;; In this case, we are consing new values to the end of the list so far,
;; producing an invalid list.

(define (square-list-c l)
  (letrec ((s (lambda (l result)
                (if (null? l)
                    result
                    (s (cdr l)
                       (append result
                               (list (square (car l)))))))))
    (s l '())))

(displayln (square-list-c '(1 2 3 4)))
;; (1 4 9 16)
