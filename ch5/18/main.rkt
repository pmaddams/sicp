#lang racket/base

; Exercise 5.18

(provide (all-defined-out))

(require racket/class
         vm)

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
          (printf "\t~a: ~a -> ~a\n" reg (register-val r) val))
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
                  (displayln (instruction-stmt inst))
                  ((instruction-proc inst))
                  (loop)))))
          (super execute)))

    (define/public (print-stats)
      (printf "---\npushes: ~a\nmax depth: ~a\nsteps: ~a\n---\n"
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
