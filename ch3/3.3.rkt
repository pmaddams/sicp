#lang sicp

(define (make-account balance secret)
  (let* ((withdraw (lambda (n)
                     (if (>= balance n)
                         (begin (set! balance (- balance n))
                                balance)
                         "insufficient funds")))
         (deposit (lambda (n)
                    (set! balance (+ balance n))))
         (dispatch (lambda (password m)
                     (if (not (eq? password secret))
                         (error "incorrect password")
                         (cond ((eq? m 'withdraw) withdraw)
                               ((eq? m 'deposit) deposit)
                               (else (error "unknown request")))))))
    (if (not (string? secret))
        (error "invalid password")
        dispatch)))

(define (displayln x)
  (display x)
  (newline))

(let ((acc (make-account 100 "12345")))
  (displayln ((acc "12345" 'withdraw) 100))
  (displayln ((acc "12345" 'withdraw) 100))
  (displayln ((acc "54321" 'withdraw) 100)))
;; 0
;; insufficient funds
;; incorrect password