#lang sicp

(define (cons x y)
  (let* ((set-x! (lambda (v)
                   (set! x v)))
         (set-y! (lambda (v)
                   (set! y v)))
         (dispatch (lambda (m)
                     (case m
                       ('car x)
                       ('cdr y)
                       ('set-car! set-x!)
                       ('set-cdr! set-y!)
                       (else (error "cons: unknown method:" m))))))
    dispatch))

(define (car z)
  (z 'car))

(define (cdr z)
  (z 'cdr))

(define (set-car! z v)
  ((z 'set-car!) v))

(define (set-cdr! z v)
  ((z 'set-cdr!) v))

(define x (cons 1 2))

(define z (cons x x))

(set-car! (cdr z) 17)

(car x)
;; 17

;;             ----------
;; global-env->|cons    |
;;             |car     |
;;             |cdr     |
;;             |set-car!|
;;             |set-cdr!|<----
;;             -^-|------    |
;;             -|-V----     -|-----
;;         E1->|x: 1  | E2->|v: 17|
;;             |y: 2  |     -------
;;             |set-x!| call to set-car!
;;             |set-y!|
;;             --------
