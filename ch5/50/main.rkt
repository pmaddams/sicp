#lang racket/base

; Exercise 5.50

(provide (all-defined-out))

(require (only-in racket (apply apply-builtin))
         racket/class
         racket/set
         "../../vm/vm.rkt")

(define (execute code)
  (let ((vm (make-vm (compile* code) #:regs all-regs)))
    (send vm set 'env (make-env))
    (send vm execute)
    (send vm get 'val)))

(define (compile* code)
  (text (compile-list code 'val 'next)))

(struct output (needs modifies text))

(struct builtin (proc))

(struct subroutine (label env))

(define (compile expr target linkage)
  (cond ((literal? expr) (compile-literal expr target linkage))
        ((symbol? expr) (compile-variable expr target linkage))
        (else (case (car expr)
                ('quote (compile-quote expr target linkage))
                ('lambda (compile-lambda expr target linkage))
                ('define (compile-define expr target linkage))
                ('set! (compile-set expr target linkage))
                ('if (compile-if expr target linkage))
                ('begin (compile-list (cdr expr) target linkage))
                ('cond (compile (cond->if expr) target linkage))
                ('and (compile (and->if expr) target linkage))
                ('or (compile (or->if expr) target linkage))
                ('let (compile (let->lambda expr) target linkage))
                (else (compile-application expr target linkage))))))

(define (literal? expr)
  (or (boolean? expr)
      (number? expr)
      (string? expr)))

(define (compile-literal expr target linkage)
  (end-with linkage
            (output '() (list target)
                    `((assign ,target (const ,expr))))))

(define (compile-variable expr target linkage)
  (end-with linkage
            (output '(env) (list target)
                    `((assign ,target (op get-var) (const ,expr) (reg env))))))

(define (compile-quote expr target linkage)
  (end-with linkage
            (output '() (list target)
                    `((assign ,target (const ,(cadr expr)))))))

(define (compile-lambda expr target linkage)
  (let ((label (gensym 'lambda-))
        (after-label (gensym 'after-lambda-)))
    (let ((linkage* (if (eq? linkage 'next) after-label linkage)))
      (append-output
       (embed-output
        (end-with linkage*
                  (output '(env) (list target)
                          `((assign ,target (op subroutine) (label ,label) (reg env)))))
        (compile-lambda-body expr label))
       after-label))))

(define (compile-lambda-body expr label)
  (let ((vars (cadr expr))
        (body (cddr expr)))
    (append-output
     (output '(env proc args) '(env)
             `(,label
               (assign env (op subroutine-env) (reg proc))
               (assign env (op subst) (const ,vars) (reg args) (reg env))))
     (compile-list body 'val 'return))))

(define (compile-define expr target linkage)
  (let* ((var (if (symbol? (cadr expr))
                  (cadr expr)
                  (caadr expr)))
         (x (if (symbol? (cadr expr))
                (caddr expr)
                (let ((vars (cdadr expr))
                      (body (cddr expr)))
                  (cons 'lambda (cons vars body)))))
         (out (compile x 'val 'next)))
    (end-with linkage
              (output-preserving
               '(env)
               out
               (output
                '(env val) (list target)
                `((perform (op define-var) (const ,var) (reg val) (reg env))
                  (assign ,target (const ok))))))))

(define (compile-set expr target linkage)
  (let ((var (cadr expr))
        (out (compile (caddr expr expr) 'val 'next)))
    (end-with linkage
              (output-preserving
               '(env)
               out
               (output
                '(env val) (list target)
                `((perform (op set-var) (const ,var) (reg val) (reg env))
                  (assign ,target (const ok))))))))

(define (compile-if expr target linkage)
  (let ((predicate (cadr expr))
        (consequent (caddr expr))
        (alternative (cadddr expr))
        (then-label (gensym 'then-))
        (else-label (gensym 'else-))
        (after-label (gensym 'after-if-)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-label linkage)))
      (let ((predicate-out (compile predicate 'val 'next))
            (consequent-out (compile consequent target consequent-linkage))
            (alternative-out (compile alternative target linkage)))
        (output-preserving
         '(env continue)
         predicate-out
         (append-output
          (output '(val) '()
                  `((test (op false?) (reg val))
                    (branch (label ,else-label))))
          (parallel-output
           (append-output then-label consequent-out)
           (append-output else-label alternative-out))
          after-label))))))

(define (compile-list exprs target linkage)
  (if (null? (cdr exprs))
      (compile (car exprs) target linkage)
      (output-preserving
       '(env continue)
       (compile (car exprs) target 'next)
       (compile-list (cdr exprs) target linkage))))

(define (cond->if expr) (expand-cond (cdr expr)))

(define (expand-cond clauses)
  (if (null? clauses)
      #f
      (let* ((first (car clauses))
             (predicate (car first))
             (consequent (list->expr (cdr first)))
             (rest (cdr clauses)))
        (if (eq? predicate 'else)
            (if (null? rest)
                consequent
                (error "else clause must be last"))
            (let ((alternative (expand-cond rest)))
              (list 'if predicate consequent alternative))))))

(define (list->expr exprs)
  (if (null? (cdr exprs))
      (car exprs)
      (cons 'begin exprs)))

(define (and->if expr) (expand-and (cdr expr)))

(define (expand-and exprs)
  (cond ((null? exprs) #t)
        ((null? (cdr exprs)) (car exprs))
        (else (let ((predicate (car exprs))
                    (consequent (expand-and (cdr exprs)))
                    (alternative #f))
                (list 'if predicate consequent alternative)))))

(define (or->if expr) (expand-or (cdr expr)))

(define (expand-or exprs)
  (cond ((null? exprs) #f)
        ((null? (cdr exprs)) (car exprs))
        (else (let ((predicate (car exprs))
                    (consequent (car exprs))
                    (alternative (expand-or (cdr exprs))))
                (list 'if predicate consequent alternative)))))

(define (let->lambda expr)
  (let* ((bindings (cadr expr))
         (vars (map car bindings))
         (exprs (map cadr bindings))
         (body (cddr expr)))
    (cons (cons 'lambda (cons vars body)) exprs)))

(define (compile-application expr target linkage)
  (let ((proc-out (compile (car expr) 'proc 'next))
        (compiled-vals (reverse (map (lambda (x) (compile x 'val 'next))
                                     (cdr expr)))))
    (output-preserving
     '(env continue)
     proc-out
     (output-preserving
      '(proc continue)
      (construct-args compiled-vals)
      (compile-call target linkage)))))

(define (construct-args compiled-vals)
  (if (null? compiled-vals)
      (output '() '(args)
              '((assign args (const ()))))
      (let ((last-out
             (append-output
              (car compiled-vals)
              (output '(val) '(args)
                      '((assign args (op list) (reg val)))))))
        (if (null? (cdr compiled-vals))
            last-out
            (output-preserving
             '(env)
             last-out
             (construct-init-args
              (cdr compiled-vals)))))))

(define (construct-init-args compiled-vals)
  (let ((next-out
         (output-preserving
          '(args)
          (car compiled-vals)
          (output '(val args) '(args)
                  '((assign args (op cons) (reg val) (reg args)))))))
    (if (null? (cdr compiled-vals))
        next-out
        (output-preserving
         '(env)
         next-out
         (construct-init-args (cdr compiled-vals))))))

(define (compile-call target linkage)
  (let ((builtin-label (gensym 'builtin-))
        (subroutine-label (gensym 'subroutine-))
        (after-label (gensym 'after-call-)))
    (let ((subroutine-linkage (if (eq? linkage 'next) after-label linkage)))
      (append-output
       (output '(proc) '()
               `((test (op builtin?) (reg proc))
                 (branch (label ,builtin-label))))
       (parallel-output
        (append-output
         subroutine-label
         (compile-subroutine-call target subroutine-linkage))
        (append-output
         builtin-label
         (end-with linkage
                   (output '(proc args) (list target)
                           `((assign ,target (op builtin-proc) (reg proc))
                             (assign ,target (op apply-builtin) (reg ,target) (reg args)))))))
       after-label))))

(define (compile-subroutine-call target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (output '(proc) all-regs
                 `((assign continue (label ,linkage))
                   (assign val (op subroutine-label) (reg proc))
                   (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (gensym 'proc-return-)))
           (output '(proc) all-regs
                   `((assign continue (label ,proc-return))
                     (assign val (op subroutine-label) (reg proc))
                     (goto (reg val))
                     ,proc-return
                     (assign ,target (reg val))
                     (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (output '(proc continue) all-regs
                 '((assign val (op subroutine-label) (reg proc))
                   (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))

(define (end-with linkage out)
  (output-preserving
   '(continue)
   out
   (case linkage
     ('next (empty-output))
     ('return (output '(continue) '()
                      '((goto (reg continue)))))
     (else (output '() '()
                   `((goto (label ,linkage))))))))

(define (output-preserving regs out1 out2)
  (if (null? regs)
      (append-output out1 out2)
      (let ((first-reg (car regs)))
        (if (and (needs? out2 first-reg)
                 (modifies? out1 first-reg))
            (output-preserving
             (cdr regs)
             (output
              (set-union (list first-reg)
                         (needed out1))
              (set-subtract (modified out1)
                            (list first-reg))
              (append `((save ,first-reg))
                      (text out1)
                      `((restore ,first-reg))))
             out2)
            (output-preserving (cdr regs) out1 out2)))))

(define (parallel-output out1 out2)
  (output
   (set-union (needed out1)
              (needed out2))
   (set-union (modified out1)
              (modified out2))
   (append (text out1) (text out2))))

(define (embed-output out body-out)
  (output
   (needed out)
   (modified out)
   (append (text out) (text body-out))))

(define (append-output . args)
  (define (combine out1 out2)
    (output (set-union (needed out1)
                       (set-subtract (needed out2)
                                     (modified out1)))
            (set-union (modified out1)
                       (modified out2))
            (append (text out1) (text out2))))

  (let loop ((l args))
    (if (null? l)
        (empty-output)
        (combine (car l) (loop (cdr l))))))

(define (empty-output)
  (output '() '() '()))

(define (needed out)
  (if (symbol? out) '() (output-needs out)))

(define (modified out)
  (if (symbol? out) '() (output-modifies out)))

(define (text out)
  (if (symbol? out) (list out) (output-text out)))

(define (needs? out reg)
  (memq reg (needed out)))

(define (modifies? out reg)
  (memq reg (modified out)))

(define all-regs '(env proc val args continue))

(define (subst vars vals env)
  (cons (make-frame vars vals) env))

(define (make-frame vars vals)
  (if (not (= (length vars) (length vals)))
      (error "arity mismatch:" vars vals)
      (let ((assocs (map cons vars vals)))
        (make-hash assocs))))

(define (define-var var val env)
  (let ((frame (car env)))
    (hash-set! frame var val)))

(define (get-var var env)
  (if (null? env)
      (error "undefined:" var)
      (let ((frame (car env)))
        (if (hash-has-key? frame var)
            (hash-ref frame var)
            (get-var var (cdr env))))))

(define (set-var var val env)
  (if (null? env)
      (error "undefined:" var)
      (let ((frame (car env)))
        (if (hash-has-key? frame var)
            (hash-set! frame var val)
            (set-var var val (cdr env))))))

(define builtins
  `((+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)
    (< . ,<)
    (> . ,>)
    (= . ,=)
    (eq? . ,eq?)
    (not . ,not)
    (null? . ,null?)
    (pair? . ,pair?)
    (cons . ,cons)
    (car . ,car)
    (cdr . ,cdr)
    (list . ,list)
    (display . ,display)
    (newline . ,newline)))

(define (make-env)
  (let ((vars (map car builtins))
        (vals (map builtin (map cdr builtins))))
    (subst vars vals '())))
