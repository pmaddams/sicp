#lang sicp

(define (make-account balance secret)
  (let* ((db (list secret))
         (authorized? (lambda (password)
                        (member password db)))
         (add (lambda (password)
                (set! db
                      (cons password db))))
         (reject (lambda (_)
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
                     (if (not (authorized? password))
                         reject
                         (case m
                           ('add add)
                           ('withdraw withdraw)
                           ('deposit deposit)
                           (else (error "account: unknown method:" m)))))))
    dispatch))

(define (make-joint account old-password new-password)
  ((account old-password 'add) new-password)
  account)

(define (displayln x)
  (display x)
  (newline))

(define (enumerate-interval low high)
  (letrec ((e (lambda (i result)
                (if (< i low)
                    result
                    (e (dec i)
                       (cons i result))))))
    (e high '())))

(let* ((peter-acc (make-account 100 'open-sesame))
       (paul-acc (make-joint peter-acc 'open-sesame 'rosebud)))
  (for-each (lambda (_)
              (displayln ((paul-acc 'rosebud 'withdraw) 50)))
            (enumerate-interval 1 3)))
;; 50
;; 0
;; insufficient funds
