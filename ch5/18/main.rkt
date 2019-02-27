#lang racket/base

; Exercise 5.18

(provide (all-defined-out))

(require racket/class
         "../../vm/vm.rkt")

(define (monitored vm%)
  (class vm%
    (super-new)

    (field (pushes 0)
           (cur-depth 0)
           (max-depth 0))

    (define/override (push val)
      (set! pushes (add1 pushes))
      (set! cur-depth (add1 cur-depth))
      (set! max-depth (max cur-depth max-depth))
      (super push val))

    (define/override (pop)
      (set! cur-depth (sub1 cur-depth))
      (super pop))

    (define/public (print-stats)
      (printf "..."))

    (define/public (reset-stats)
      (set! pushes 0)
      (set! max-depth 0)
      (set! cur-depth 0))))
