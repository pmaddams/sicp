#lang sicp

(define (make-withdraw-a balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insufficient funds")))

(define (displayln x)
  (display x)
  (newline))

(let ((W1 (make-withdraw-a 100))
      (W2 (make-withdraw-a 100)))
  (displayln (W1 50))
  (displayln (W2 0)))
;; 50
;; 100

;;             -----------------
;; global-env->|make-withdraw-a|
;;             -^---------------
;;             -|------------  ------------
;;         E1->|balance: 100|<-|amount: 50|
;;             --------------  ------------
;;         (lambda (amount)
;;          (if (>= balance amount)
;;              (begin (set! balance (- balance amount))
;;                     balance)
;;              "insufficient funds"))

(define (make-withdraw-b initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "insufficient funds"))))

(let ((W1 (make-withdraw-b 100))
      (W2 (make-withdraw-b 100)))
  (displayln (W1 50))
  (displayln (W2 0)))
;; 50
;; 100

;;             -----------------
;; global-env->|make-withdraw-b|
;;             -^---------------
;;             -|--------------
;;         E1->|initial-amount| (let ((balance initial-amount)) ...)
;;             -^--------------
;;             -|------------  ------------
;;         E2->|balance: 100|<-|amount: 50|
;;             --------------  ------------
;;         (lambda (amount)
;;           (if (>= balance amount)
;;               (begin (set! balance (- balance amount))
;;                      balance)
;;               "insufficient funds"))))

;; The second version of make-withdraw creates an environment with an additional
;; frame representing the let expression which sets balance to initial-amount.
