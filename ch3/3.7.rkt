#lang sicp

(define (make-account balance initial-password)
  (let* ((password-db (list initial-password))
         (check-password (lambda (password)
                           (letrec ((c (lambda (db)
                                         (if (null? db)
                                             #f
                                             (or (eq? password (car db))
                                                 (c (cdr db)))))))
                             (c password-db))))
         (add-password (lambda (new-password)
                         (set! password-db
                               (cons new-password password-db))))
         (withdraw (lambda (n)
                     (if (>= balance n)
                         (begin (set! balance (- balance n))
                                balance)
                         "insufficient funds")))
         (deposit (lambda (n)
                    (set! balance (+ balance n))))
         (dispatch (lambda (password m)
                     (if (not (check-password password))
                         (error "incorrect password")
                         (case m
                           ('add-password add-password)
                           ('withdraw withdraw)
                           ('deposit deposit)
                           (else (error "account: unknown method:" m)))))))
    (if (not (symbol? initial-password))
        (error "invalid password")
        dispatch)))

(define (make-joint account password new-password)
  ((account password 'add-password) new-password)
  account)

(define (displayln x)
  (display x)
  (newline))

(let* ((peter-acc (make-account 100 'open-sesame))
       (paul-acc (make-joint peter-acc 'open-sesame 'rosebud)))
  (displayln ((paul-acc 'rosebud 'withdraw) 50))
  (displayln ((paul-acc 'rosebud 'withdraw) 50))
  (displayln ((paul-acc 'rosebud 'withdraw) 50)))
;; 50
;; 0
;; insufficient funds
