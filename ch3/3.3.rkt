#lang sicp

(define (make-account balance secret)
  (let* ((reject (lambda (_)
                   "incorrect password"))
         (withdraw (lambda (n)
                     (if (>= balance n)
                         (begin (set! balance
                                      (- balance n))
                                balance)
                         "insufficient funds")))
         (deposit (lambda (n)
                    (set! balance
                          (+ balance n))
                    balance))
         (dispatch (lambda (password m)
                     (if (not (equal? password secret))
                         reject
                         (case m
                           ('withdraw withdraw)
                           ('deposit deposit)
                           (else (error "account: unknown method:" m)))))))
    dispatch))

(define (displayln x)
  (display x)
  (newline))

(let ((acc (make-account 100 "12345")))
  (for-each (lambda (password)
              (displayln ((acc password 'withdraw) 100)))
            '("12345"
              "12345"
              "54321")))
;; 0
;; insufficient funds
;; incorrect password
