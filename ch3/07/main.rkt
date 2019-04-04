#lang racket/base

; Exercise 3.7

(provide (all-defined-out))

(require racket/class)

(define account%
  (class object%
    (super-new)

    (init password)
    (init-field (balance 0))
    (field (passwords '()) (attempts 0))

    (define/public (deposit password n)
      (or (invalid? password)
          (begin (set! balance (+ balance n))
                 balance)))

    (define/public (withdraw password n)
      (or (invalid? password)
          (if (>= balance n)
              (begin (set! balance (- balance n))
                     balance)
              "insufficient funds")))

    (define/public (add-user password new-password)
      (or (invalid? password)
          (if (member new-password passwords)
              "user exists"
              (begin (set! passwords (cons new-password passwords))
                     "added user"))))

    (define (invalid? password)
      (if (or (null? passwords)
              (member password passwords))
          (begin (set! attempts 0) #f)
          (begin (set! attempts (add1 attempts))
                 (if (> attempts 7)
                     (call-the-cops)
                     "invalid password"))))

    (define (call-the-cops) "dialing 911")

    (send this add-user (void) password)))
