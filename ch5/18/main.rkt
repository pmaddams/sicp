#lang racket/base

; Exercise 5.18

(provide (all-defined-out))

(require racket/class
         "../../vm/vm.rkt")

(define (tracing vm%)
  (class vm%
    (super-new)

    (field (pushes 0)
           (cur-depth 0)
           (max-depth 0)
           (steps 0)
           (trace-insts #f)
           (trace-regs '()))

    (define/override (push val)
      (set! pushes (add1 pushes))
      (set! cur-depth (add1 cur-depth))
      (set! max-depth (max cur-depth max-depth))
      (super push val))

    (define/override (pop)
      (set! cur-depth (sub1 cur-depth))
      (super pop))

    (define/override (set reg val)
      (let ((r (hash-ref (get-field reg-table this) reg)))
        (when (memq reg trace-regs)
          (printf "~a: ~a -> ~a\n" reg (register-val r) val))
        (set-register-val! r val)))

    (define/override (advance)
      (set! steps (add1 steps))
      (super advance))

    (define/override (execute)
      (if trace-insts
          (let loop ()
            (let ((insts (send this get 'pc)))
              (unless (null? insts)
                (let ((inst (car insts)))
                  (displayln (instruction-expr inst))
                  ((instruction-proc inst))
                  (loop)))))
          (super execute)))

    (define/public (print-stats)
      (printf "pushes: ~a\nmax depth: ~a\nsteps: ~a\n"
              pushes max-depth steps))

    (define/public (reset-stats)
      (set! pushes 0)
      (set! max-depth 0)
      (set! cur-depth 0)
      (set! steps 0))

    (define/public (reg-trace-on reg)
      (unless (memq reg trace-regs)
        (set! trace-regs (cons reg trace-regs))))

    (define/public (reg-trace-off reg)
      (set! trace-regs (remq reg trace-regs)))

    (define/public (inst-trace-on)
      (set! trace-insts #t))

    (define/public (inst-trace-off)
      (set! trace-insts #f))))

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

  (let ((vm (make-vm code #:type (tracing vm%))))
    (send vm set 'a a)
    (send vm set 'n n)
    (send vm inst-trace-on)
    (send vm reg-trace-on 'a)
    (send vm reg-trace-on 'n)
    (send vm reg-trace-on 'val)
    (send vm execute)
    (send vm print-stats)
    (send vm get 'val)))
