#lang sicp

(define (count-change amount)
  (let ((first-denomination
         (lambda (kinds-of-coins)
           (case kinds-of-coins
             ((1) 1)
             ((2) 5)
             ((3) 10)
             ((4) 25)
             ((5) 50)))))
    (letrec ((cc (lambda (amount kinds-of-coins)
                   (cond ((zero? amount) 1)
                         ((or (negative? amount)
                              (zero? kinds-of-coins)) 0)
                         (else (+ (cc amount
                                      (dec kinds-of-coins))
                                  (cc (- amount
                                         (first-denomination kinds-of-coins))
                                      kinds-of-coins)))))))
      (cc amount 5))))

(count-change 11)
;; 4

;; (cc 11 5)
;; (cc 11 4) (cc -39 5)
;; (cc 11 3) (cc -14 4) 0
;; (cc 11 2) (cc 1 3) 0
;; (cc 11 1) (cc 6 2) (cc 1 2) (cc -9 3)
;; (cc 11 0) (cc 10 1) (cc 6 1) (cc 1 2) (cc 1 1) (cc -4 2) 0
;; 0 (cc 10 0) (cc 9 1) (cc 6 0) (cc 5 1) (cc 1 1) (cc -4 2) (cc 1 0) (cc 0 1) 0
;; 0 (cc 9 0) (cc 8 1) 0 (cc 5 0) (cc 4 1) (cc 1 0) (cc 0 1) 0 1
;; 0 (cc 8 0) (cc 7 1) 0 (cc 4 0) (cc 3 1) 0 1
;; 0 (cc 7 0) (cc 6 1) 0 (cc 3 0) (cc 2 1)
;; 0 (cc 6 0) (cc 5 1) 0 (cc 2 0) (cc 1 1)
;; 0 (cc 5 0) (cc 4 1) 0 (cc 1 0) (cc 0 1)
;; 0 (cc 4 0) (cc 3 1) 0 1
;; 0 (cc 3 0) (cc 2 1)
;; 0 (cc 2 0) (cc 1 1)
;; 0 (cc 1 0) (cc 0 1)
;; 0 1

;; Four branches of the tree evaluate to 1 and the rest evaluate to 0.
;; Therefore, the answer is 4.

;; The order of growth with respect to space is theta(amount).
;; The order of growth with respect to time is theta(amount ^ kinds-of-coins).
