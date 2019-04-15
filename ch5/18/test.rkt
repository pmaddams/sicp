#lang racket/base

(require racket/class
         racket/sequence
         rackunit
         vm
         "main.rkt")

(test-case
 "recursive factorial"
 (define code
   '((assign continue (label done))
     loop
     (test (op <=) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-loop))
     (goto (label loop))
     after-loop
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     done))

 (let ((vm (new (tracing vm%)
                (regs (needed-regs code))
                (ops (needed-ops code)))))
   (send vm reg-trace-on 'n)
   (send vm reg-trace-on 'val)
   (send vm inst-trace-on)
   (for ((n (in-range 10)))
     (send vm set 'n n)
     (send vm install code)
     (send vm execute)
     (send vm print-stats)
     (send vm reset-stats)
     (check-equal? (send vm get 'val)
                   (sequence-fold * 1 (in-range 1 (add1 n)))))))

(test-case
 "recursive fibonacci"
 (define code
   '((assign continue (label done))
     loop
     (test (op <) (reg n) (const 2))
     (branch (label base-case))
     (save continue)
     (assign continue (label after-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label loop))
     after-n-1
     (restore n)
     (restore continue)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label after-n-2))
     (save val)
     (goto (label loop))
     after-n-2
     (assign n (reg val))
     (restore val)
     (restore continue)
     (assign val (op +) (reg val) (reg n))
     (goto (reg continue))
     base-case
     (assign val (reg n))
     (goto (reg continue))
     done))

 (let ((vm (new (tracing vm%)
                (regs (needed-regs code))
                (ops (needed-ops code)))))
   (send vm reg-trace-on 'n)
   (send vm reg-trace-on 'val)
   (send vm inst-trace-on)
   (check-equal?
    (for/list ((n (in-range 10)))
      (send vm set 'n n)
      (send vm install code)
      (send vm execute)
      (send vm print-stats)
      (send vm reset-stats)
      (send vm get 'val))
    '(0 1 1 2 3 5 8 13 21 34))))
