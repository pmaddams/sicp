#lang sicp

(define (count-change amount)
  (letrec ((cc (lambda (amount kinds-of-coins)
                 (cond ((zero? amount) 1)
                       ((or (negative? amount)
                            (zero? kinds-of-coins)) 0)
                       (else (+ (cc amount
                                    (dec kinds-of-coins))
                                (cc (- amount
                                       (first-denomination kinds-of-coins))
                                    kinds-of-coins))))))
           (first-denomination (lambda (kinds-of-coins)
                                 (case kinds-of-coins
                                   ((1) 1)
                                   ((2) 5)
                                   ((3) 10)
                                   ((4) 25)
                                   ((5) 50)))))
    (cc amount 5)))