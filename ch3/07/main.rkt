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
      (when (check-password password)
        (set! balance (+ balance n))
        balance))

    (define/public (withdraw password n)
      (when (check-password password)
        (if (>= balance n)
            (begin (set! balance (- balance n))
                   balance)
            "insufficient funds")))

    (define/public (add-user current-password new-password)
      (when (check-password current-password)
        (unless (member new-password passwords)
          (set! passwords (cons new-password passwords)))))

    (define (check-password password)
      (if (or (null? passwords) (member password passwords))
          (begin (set! attempts 0)
                 #t)
          (begin (set! attempts (add1 attempts))
                 (when (> attempts 7)
                   (call-the-cops))
                 #f)))

    (define (call-the-cops)
      (error "dialing 911..."))

    (send this add-user (void) password)))
