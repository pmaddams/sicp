#lang racket/base

(require racket/class
         racket/sequence
         rackunit
         "main.rkt")

(define-namespace-anchor here)

(define rem remainder) ; test local definitions

(define (assoc<? a b)
  (symbol<? (car a) (car b)))

(test-case
 "gcd with builtin"
 (define code
   '(loop
     (test (op =) (reg b) (const 0))
     (branch (label done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label loop))
     done))

 (check-equal? (used-in code 'assign)'(t a b))
 (check-equal? (used-in code 'reg) '(b a b b t))
 (check-equal? (used-in code 'op) '(= rem))
 (check-equal? (sort (needed-regs code) symbol<?)
               '(a b t))
 (check-equal? (sort (needed-ops code #:in here) assoc<?)
               `((= . ,=) (rem . ,rem)))

 (for ((i (in-range 5)))
   (let ((vm (make-vm code #:in here))
         (a (random 1 100))
         (b (random 1 100)))
     (send vm set 'a a)
     (send vm set 'b b)
     (send vm execute)
     (check-equal? (send vm get 'a)
                   (gcd a b)))))

(test-case
 "gcd with subroutine"
 (define code
   '(gcd-loop
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (reg a))
     rem-loop
     (test (op <) (reg t) (reg b))
     (branch (label rem-done))
     (assign t (op -) (reg t) (reg b))
     (goto (label rem-loop))
     rem-done
     (assign a (reg b))
     (assign b (reg t))
     (goto (label gcd-loop))
     gcd-done))

 (for ((i (in-range 5)))
   (let ((vm (make-vm code))
         (a (random 1 100))
         (b (random 1 100)))
     (send vm set 'a a)
     (send vm set 'b b)
     (send vm execute)
     (check-equal? (send vm get 'a)
                   (gcd a b)))))

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

 (for ((n (in-range 10)))
   (let ((vm (make-vm code)))
     (send vm set 'n n)
     (send vm execute)
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

 (check-equal?
  (for/list ((n (in-range 10)))
    (let ((vm (make-vm code)))
      (send vm set 'n n)
      (send vm execute)
      (send vm get 'val)))
  '(0 1 1 2 3 5 8 13 21 34)))
