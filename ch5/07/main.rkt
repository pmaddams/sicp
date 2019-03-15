#lang racket/base

; Exercise 5.7

(provide (all-defined-out))

(require racket/class
         "../../vm/vm.rkt")

(define (expt-rec a n)
  (define code
    '((assign continue (label done))
      loop
      (test (op zero?) (reg n))
      (branch (label base-case))
      (assign n (op sub1) (reg n))
      (save continue)
      (assign continue (label after-loop))
      (goto (label loop))
      after-loop
      (restore continue)
      (assign val (op *) (reg val) (reg a))
      (goto (reg continue))
      base-case
      (assign val (const 1))
      (goto (reg continue))
      done))

  (let ((vm (make-vm code)))
    (send vm set 'a a)
    (send vm set 'n n)
    (send vm execute)
    (send vm get 'val)))

(define (expt-iter a n)
  (define code
    '((assign val (const 1))
      loop
      (test (op zero?) (reg n))
      (branch (label done))
      (assign val (op *) (reg a) (reg val))
      (assign n (op sub1) (reg n))
      (goto (label loop))
      done))

  (let ((vm (make-vm code)))
    (send vm set 'a a)
    (send vm set 'n n)
    (send vm execute)
    (send vm get 'val)))
