#lang sicp

(define (make-account balance secret)
  (let* ((warnings 0)
         (call-the-cops (lambda ()
                          (error "robocop activated, please stand by")))
         (log-attempt (lambda (_)
                        (set! warnings (inc warnings))
                        (if (<= warnings 7)
                            (string-append "you have made "
                                           (number->string warnings)
                                           " incorrect attempt"
                                           (if (> warnings 1) "s" ""))
                            (call-the-cops))))
         (withdraw (lambda (n)
                     (if (>= balance n)
                         (begin (set! balance (- balance n))
                                balance)
                         "insufficient funds")))
         (deposit (lambda (n)
                    (set! balance (+ balance n))))
         (dispatch (lambda (password m)
                     (if (not (eq? password secret))
                         log-attempt
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
  (displayln ((acc "we're" 'withdraw) 100))
  (displayln ((acc "not" 'withdraw) 100))
  (displayln ((acc "gonna" 'withdraw) 100))
  (displayln ((acc "take" 'withdraw) 100))
  (displayln ((acc "it" 'withdraw) 100))
  (displayln ((acc "no" 'withdraw) 100))
  (displayln ((acc "we" 'withdraw) 100))
  (displayln ((acc "won't" 'withdraw) 100)))
;; you have made 1 incorrect attempt
;; you have made 2 incorrect attempts
;; you have made 3 incorrect attempts
;; you have made 4 incorrect attempts
;; you have made 5 incorrect attempts
;; you have made 6 incorrect attempts
;; you have made 7 incorrect attempts
;; robocop activated, please stand by