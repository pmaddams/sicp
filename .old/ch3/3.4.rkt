#lang sicp

(define (make-account balance secret)
  (let* ((warnings 0)
         (call-the-cops (lambda ()
                          (error "robocop activated")))
         (reject (lambda (_)
                   (set! warnings
                         (inc warnings))
                   (if (< warnings 8)
                       "incorrect password"
                       (call-the-cops))))
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
                         (begin
                           (set! warnings 0)
                           (case m
                             ('withdraw withdraw)
                             ('deposit deposit)
                             (else (error "account: unknown method:" m))))))))
    dispatch))

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

(let ((acc (make-account 100 "12345")))
  (for-each (lambda (password)
              (displayln ((acc password 'withdraw) 100)))
            (enumerate-interval 1 8)))
;; incorrect password
;; incorrect password
;; incorrect password
;; incorrect password
;; incorrect password
;; incorrect password
;; incorrect password
;; robocop activated
