#lang sicp

(define (make-account balance)
  (let* ((withdraw (lambda (n)
                     (if (>= balance n)
                         (begin (set! balance (- balance n))
                                balance)
                         "insufficient funds")))
         (deposit (lambda (n)
                    (set! balance (+ balance n))
                    balance))
         (dispatch (lambda (m)
                     (case m
                       ('withdraw withdraw)
                       ('deposit deposit)
                       (else (error "account: unknown method:" m))))))
    dispatch))

(define (displayln x)
  (display x)
  (newline))

(let ((acc (make-account 50)))
  (displayln ((acc 'deposit) 40))
  (displayln ((acc 'withdraw) 60)))
;; 90
;; 30

;;             --------------
;; global-env->|make-account|
;;             -^------------
;;             -|-----------
;;         E1->|balance: 50|
;;             |withdraw   |
;;             |deposit    |
;;             |dispatch   |<-----
;;             -^-----------     |
;;             -|----------     -|----------
;;         E2->|amount: 40| E3->|amount: 60|
;;             ------------     ------------
;;         call to deposit  call to withdraw

;; The local state for an account is kept in the first frame generated from the
;; global environment. Only the implementation of the procedure make-account is
;; shared between accounts.
