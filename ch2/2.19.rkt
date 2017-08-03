#lang sicp

(define (cc amount coin-values)
  (let ((first-denomination car)
        (except-first-denomination cdr)
        (no-more? null?))
    (cond ((zero? amount) 1)
          ((or (negative? amount)
               (no-more? coin-values)) 0)
          (else (+ (cc amount
                       (except-first-denomination coin-values))
                   (cc (- amount
                          (first-denomination coin-values))
                       coin-values))))))

(define us-coins '(50 25 10 5 1))

(define uk-coins '(100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)
;; 292

(cc 100 uk-coins)
;; 104561

(let ((us-coins '(1 50 5 25 10)))
  (cc 100 us-coins))
;; 292

;; The algorithm depends on the observation that "the ways to make change can be
;; divided into two groups: those that do not use any of the first kind of coin,
;; and those that do." This observation does not change depending on the order
;; of the coins. It does change, however, if the same kind of coin is repeated,
;; or if kinds of coins are added or removed from the list.
