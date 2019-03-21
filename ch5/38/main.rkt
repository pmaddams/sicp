#lang racket/base

; Exercise 5.38

(provide (all-defined-out))

(require racket/set
         (only-in racket (apply apply-builtin)))

(struct output (needed modified text))

(struct builtin (proc))

(struct compiled-closure (entry env))

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
  (let ((entry (gensym 'entry))
        (after-lambda (gensym 'after-lambda)))
    (let ((linkage* (if (eq? linkage 'next) after-lambda linkage)))
      (append-output
       (embed-output (end-with linkage*
                               (output '(env) (list target)
                                       `((assign ,target (op compiled-closure) (label ,entry) (reg env)))))
                     (compile-lambda-body expr entry))
       after-lambda))))

(define (compile-lambda-body expr entry)
  (let ((vars (cadr expr))
        (body (cddr expr)))
    (append-output
     (output '(env proc args) '(env)
             `(,entry
               (assign env (op compiled-closure-env) (reg proc))
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
              (output-preserving '(env)
                                 out
                                 (output
                                  '(env val) (list target)
                                  `((perform (op define-var) (const ,var) (reg val) (reg env))
                                    (assign ,target (const ok))))))))

(define (compile-set expr target linkage)
  (let ((var (cadr expr))
        (out (compile (caddr expr expr) 'val 'next)))
    (end-with linkage
              (output-preserving '(env)
                                 out
                                 (output
                                  '(env val) (list target)
                                  `((perform (op set-var) (const ,var) (reg val) (reg env))
                                    (assign ,target (const ok))))))))

(define (compile-if expr target linkage)
  (let ((predicate (cadr expr))
        (consequent (caddr expr))
        (alternative (cadddr expr))
        (then-branch (gensym 'then))
        (else-branch (gensym 'else))
        (after-if (gensym 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((predicate-out (compile predicate 'val 'next))
            (consequent-out (compile consequent target consequent-linkage))
            (alternative-out (compile alternative target linkage)))
        (output-preserving '(env continue)
                           predicate-out
                           (append-output
                            (output '(val) '()
                                    `((test (op false?) (reg val))
                                      (branch (label ,else-branch))))
                            (parallel-output
                             (append-output then-branch consequent-out)
                             (append-output else-branch alternative-out))
                            after-if))))))

(define (compile-list exprs target linkage)
  (if (null? (cdr exprs))
      (compile (car exprs) target linkage)
      (output-preserving '(env continue)
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
        (args-out (map (lambda (x) (compile x 'val 'next))
                       (cdr expr))))
    (output-preserving
     '(env continue)
     proc-out
     (output-preserving
      '(proc continue)
      (construct-arglist args-out)
      (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (output '() '(args)
                '((assign args (const ()))))
        (let ((code-to-get-last-arg
               (append-output
                (car operand-codes)
                (output '(val) '(args)
                        '((assign args (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (output-preserving '(env)
                                 code-to-get-last-arg
                                 (code-to-get-rest-args
                                  (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (output-preserving '(args)
                            (car operand-codes)
                            (output '(val args) '(args)
                                    '((assign args (op cons) (reg val) (reg args)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (output-preserving '(env)
                           code-for-next-arg
                           (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (gensym 'primitive-branch))
        (compiled-branch (gensym 'compiled-branch))
        (after-call (gensym 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-output
       (output '(proc) '()
               `((test (op builtin?) (reg proc))
                 (branch (label ,primitive-branch))))
       (parallel-output
        (append-output
         compiled-branch
         (compile-closure-application target compiled-linkage))
        (append-output
         primitive-branch
         (end-with linkage
                   (output '(proc args) (list target)
                           `((assign ,target (op builtin-proc) (reg proc))
                             (assign ,target (op apply-builtin) (reg ,target) (reg args)))))))
       after-call))))

(define (compile-closure-application target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (output '(proc) all-regs
                 `((assign continue (label ,linkage))
                   (assign val (op compiled-closure-entry) (reg proc))
                   (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (gensym 'proc-return)))
           (output '(proc) all-regs
                   `((assign continue (label ,proc-return))
                     (assign val (op compiled-closure-entry) (reg proc))
                     (goto (reg val))
                     ,proc-return
                     (assign ,target (reg val))
                     (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (output '(proc continue) all-regs
                 '((assign val (op compiled-closure-entry) (reg proc))
                   (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))

(define (empty-output)
  (output '() '() '()))

(define (compile-linkage linkage)
  (case linkage
    ('next (empty-output))
    ('return (output '(continue) '()
                     '((goto (reg continue)))))
    (else (output '() '()
                  `((goto (label ,linkage)))))))

(define (end-with linkage out)
  (output-preserving '(continue)
                     out
                     (compile-linkage linkage)))

(define all-regs '(env proc val args continue))

(define (needed s)
  (if (symbol? s) '() (output-needed s)))

(define (modified s)
  (if (symbol? s) '() (output-modified s)))

(define (text s)
  (if (symbol? s) (list s) (output-text s)))

(define (needs? out reg)
  (memq reg (needed out)))

(define (modifies? out reg)
  (memq reg (modified out)))

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

(define (embed-output out body-out)
  (output
   (needed out)
   (modified out)
   (append (text out) (text body-out))))

(define (parallel-output out1 out2)
  (output
   (set-union (needed out1)
              (needed out2))
   (set-union (modified out1)
              (modified out2))
   (append (text out1) (text out2))))

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
