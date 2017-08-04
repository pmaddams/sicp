#lang sicp

(define (square x)
  (expt x 2))

(define (displayln x)
  (begin (display x)
         (newline)))

(define (square-list-a items)
  (letrec ((iter (lambda (things answer)
                   (if (null? things)
                       answer
                       (iter (cdr things)
                             (cons (square (car things))
                                   answer))))))
    (iter items '())))

(displayln (square-list-a '(1 2 3 4)))
;; (16 9 4 1)

;; This procedure conses each squared item in the list to the front of the list
;; so far, so the list ends up in reverse order.

(define (square-list-b items)
  (letrec ((iter (lambda (things answer)
                   (if (null? things)
                       answer
                       (iter (cdr things)
                             (cons answer
                                   (square (car things))))))))
    (iter items '())))

(displayln (square-list-b '(1 2 3 4)))
;; ((((() . 1) . 4) . 9) . 16)

;; In this case, we are consing new values to the end of the list so far,
;; producing an invalid list.

(define (square-list-c items)
  (letrec ((iter (lambda (things answer)
                   (if (null? things)
                       answer
                       (iter (cdr things)
                             (append answer
                                     (list (square (car things)))))))))
    (iter items '())))

(displayln (square-list-c '(1 2 3 4)))
;; (1 4 9 16)