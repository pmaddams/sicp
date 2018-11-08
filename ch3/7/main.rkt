#lang racket/base

; Exercise 3.7: Bank accounts

(require racket/class)

(define (make-joint account user-password new-password)
  (send account authorize user-password new-password)
  account)

(define account%
  (class object%
    (super-new)

    (init password)
    (init-field (balance 0))
    (field (passwords '()) (attempts 0))

    (define/public (deposit user-password n)
      (when (valid? user-password)
        (set! balance (+ balance n))
        balance))

    (define/public (withdraw user-password n)
      (when (valid? user-password)
        (if (>= balance n)
            (begin (set! balance (- balance n))
                   balance)
            "insufficient funds")))

    (define/public (authorize user-password new-password)
      (when (valid? user-password)
        (unless (member new-password passwords)
          (set! passwords (cons new-password passwords)))))

    (define max-attempts 7)

    (define (valid? user-password)
      (if (or (null? passwords) (member user-password passwords))
          (begin (set! attempts 0)
                 #t)
          (begin (set! attempts (add1 attempts))
                 (when (> attempts max-attempts)
                   (call-the-cops))
                 #f)))

    (define (call-the-cops)
      (error "dialing 911..."))

    (send this authorize (void) password)))
