#lang racket/base

; Exercise 5.23

(provide (all-defined-out))

(require "../../vm/vm.rkt")

(require (only-in racket (apply builtin-apply)))

(struct builtin (proc))

(struct closure (vars body env))

(define (literal? expr)
  (or (boolean? expr)
      (number? expr)
      (string? expr)))

(define (quote? expr)
  (tagged-list? expr 'quote))

(define (tagged-list? x tag)
  (and (pair? x)
       (eq? tag (car x))))

(define (set? expr)
  (tagged-list? expr 'set!))

(define set-expr-var cadr)

(define set-expr-val caddr)

(define (define? expr)
  (tagged-list? expr 'define))

(define (define-expr-var expr)
  (if (symbol? (cadr expr))
      (cadr expr)
      (caadr expr)))

(define (define-expr-val expr)
  (if (symbol? (cadr expr))
      (caddr expr)
      (closure (cdadr expr)
               (cddr expr))))

(define (lambda? expr)
  (tagged-list? expr 'lambda))

(define (if? expr)
  (tagged-list? expr 'if))

(define if-expr-predicate cadr)

(define if-expr-consequent caddr)

(define if-expr-alternative cadddr)

(define (begin? expr)
  (tagged-list? expr 'begin))

(define (last-expr? seq) (null? (cdr seq)))
(define first-expr car)
(define rest-exprs cdr)

(define application? pair?)
(define operator car)
(define operands cdr)

(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

(define (true? x) (not (false? x)))

(define (false? x) (eq? x #f))

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

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))

(define the-global-environment (make-env))

(define (get-global-environment)
  the-global-environment)

(define interpreter
  '(loop
    (assign expr (op read))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (label eval))
    print-result
    (perform (op displayln) (reg val))
    (goto (label loop))
    unknown-expression-type
    (assign val (const "error: unknown expression type"))
    (goto (label signal-error))
    unknown-procedure-type
    (restore continue)
    (assign val (const "error: unknown procedure type"))
    (goto (label signal-error))
    signal-error
    (perform (op displayln) (reg val))
    (goto (label loop))
    eval
    (test (op literal?) (reg expr))
    (branch (label eval-self-eval))
    (test (op symbol?) (reg expr))
    (branch (label eval-variable))
    (test (op quote?) (reg expr))
    (branch (label eval-quoted))
    (test (op set?) (reg expr))
    (branch (label eval-set))
    (test (op define?) (reg expr))
    (branch (label eval-define))
    (test (op if?) (reg expr))
    (branch (label eval-if))
    (test (op lambda?) (reg expr))
    (branch (label eval-lambda))
    (test (op begin?) (reg expr))
    (branch (label eval-begin))
    (test (op application?) (reg expr))
    (branch (label eval-application))
    (goto (label unknown-expression-type))
    eval-self-eval
    (assign val (reg expr))
    (goto (reg continue))
    eval-variable
    (assign val (op get-var) (reg expr) (reg env))
    (goto (reg continue))
    eval-quoted
    (assign val (op cadr) (reg expr))
    (goto (reg continue))
    eval-lambda
    (assign unev (op closure-vars) (reg expr))
    (assign expr (op closure-body) (reg expr))
    (assign val (op closure) (reg unev) (reg expr) (reg env))
    (goto (reg continue))
    eval-application
    (save continue)
    (save env)
    (assign unev (op operands) (reg expr))
    (save unev)
    (assign expr (op operator) (reg expr))
    (assign continue (label eval-appl-did-operator))
    (goto (label eval))
    eval-appl-did-operator
    (restore unev)
    (restore env)
    (assign args (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label apply))
    (save proc)
    eval-appl-operand-loop
    (save args)
    (assign expr (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label eval-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label eval-appl-accumulate-arg))
    (goto (label eval))
    eval-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore args)
    (assign args (op adjoin-arg) (reg val) (reg args))
    (assign unev (op rest-operands) (reg unev))
    (goto (label eval-appl-operand-loop))
    eval-appl-last-arg
    (assign continue (label eval-appl-accum-last-arg))
    (goto (label eval))
    eval-appl-accum-last-arg
    (restore args)
    (assign args (op adjoin-arg) (reg val) (reg args))
    (restore proc)
    (goto (label apply))
    apply
    (test (op builtin?) (reg proc))
    (branch (label primitive-apply))
    (test (op closure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))
    primitive-apply
    (assign val (op builtin-apply) (reg proc) (reg args))
    (restore continue)
    (goto (reg continue))
    compound-apply
    (assign unev (op closure-vars) (reg proc))
    (assign env (op closure-env) (reg proc))
    (assign env (op subst) (reg unev) (reg args) (reg env))
    (assign unev (op closure-body) (reg proc))
    (goto (label eval-sequence))
    eval-begin
    (assign unev (op cdr) (reg expr))
    (save continue)
    (goto (label eval-sequence))
    eval-sequence
    (assign expr (op first-expr) (reg unev))
    (test (op last-expr?) (reg unev))
    (branch (label eval-sequence-last-expr))
    (save unev)
    (save env)
    (assign continue (label eval-sequence-continue))
    (goto (label eval))
    eval-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exprs) (reg unev))
    (goto (label eval-sequence))
    eval-sequence-last-expr
    (restore continue)
    (goto (label eval))
    eval-if
    (save expr)
    (save env)
    (save continue)
    (assign continue (label eval-if-decide))
    (assign expr (op if-expr-predicate) (reg expr))
    (goto (label eval))
    eval-if-decide
    (restore continue)
    (restore env)
    (restore expr)
    (test (op true?) (reg val))
    (branch (label eval-if-consequent))
    eval-if-alternative
    (assign expr (op if-expr-alternative) (reg expr))
    (goto (label eval))
    eval-if-consequent
    (assign expr (op if-expr-consequent) (reg expr))
    (goto (label eval))
    eval-set
    (assign unev (op set-expr-var) (reg expr))
    (save unev)
    (assign expr (op set-expr-val) (reg expr))
    (save env)
    (save continue)
    (assign continue (label eval-set-1))
    (goto (label eval))
    eval-set-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op set-var) (reg unev) (reg val) (reg env))
    (assign val (const "ok"))
    (goto (reg continue))
    eval-define
    (assign unev (op define-expr-var) (reg expr))
    (save unev)
    (assign expr (op define-expr-val) (reg expr))
    (save env)
    (save continue)
    (assign continue (label eval-define-1))
    (goto (label eval))
    eval-define-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op define-var) (reg unev) (reg val) (reg env))
    (assign val (const "ok"))
    (goto (reg continue))))
