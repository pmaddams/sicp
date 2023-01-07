#lang racket/base

; Exercise 5.7

(provide (all-defined-out))

(require racket/class
         vm)

(define ((make-expt code) b n)
  (let ((vm (make-vm code)))
    (send vm set 'b b)
    (send vm set 'n n)
    (send vm execute)
    (send vm get 'val)))

(define recursive-process
  '((assign continue (label done))
    loop
    (test (op zero?) (reg n))
    (branch (label base-case))
    (save continue)
    (assign n (op sub1) (reg n))
    (assign continue (label after-loop))
    (goto (label loop))
    after-loop
    (restore continue)
    (assign val (op *) (reg b) (reg val))
    (goto (reg continue))
    base-case
    (assign val (const 1))
    (goto (reg continue))
    done))

(define iterative-process
  '((assign val (const 1))
    loop
    (test (op zero?) (reg n))
    (branch (label done))
    (assign val (op *) (reg b) (reg val))
    (assign n (op sub1) (reg n))
    (goto (label loop))
    done))
