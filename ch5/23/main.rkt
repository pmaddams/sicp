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

(define (quoted? expr)
  (tagged-list? expr 'quote))

(define (text-of-quotation expr) (cadr expr))

(define (tagged-list? expr tag)
  (if (pair? expr)
      (eq? (car expr) tag)
      #f))

(define (assignment? expr)
  (tagged-list? expr 'set!))

(define (assignment-variable expr) (cadr expr))

(define (assignment-value expr) (caddr expr))

(define (definition? expr)
  (tagged-list? expr 'define))

(define (definition-variable expr)
  (if (symbol? (cadr expr))
      (cadr expr)
      (caadr expr)))

(define (definition-value expr)
  (if (symbol? (cadr expr))
      (caddr expr)
      (closure (cdadr expr)
               (cddr expr))))

(define (lambda? expr) (tagged-list? expr 'lambda))

(define (if? expr) (tagged-list? expr 'if))

(define (if-predicate expr) (cadr expr))

(define (if-consequent expr) (caddr expr))

(define (if-alternative expr)
  (if (not (null? (cdddr expr)))
      (cadddr expr)
      'false))

(define (begin? expr) (tagged-list? expr 'begin))
(define (begin-actions expr) (cdr expr))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-expr seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (application? expr) (pair? expr))
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

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

(define (lookup-var var env)
  (if (null? env)
      (error "undefined:" var)
      (let ((frame (car env)))
        (if (hash-has-key? frame var)
            (hash-ref frame var)
            (lookup-var var (cdr env))))))

(define (assign-var var val env)
  (if (null? env)
      (error "undefined:" var)
      (let ((frame (car env)))
        (if (hash-has-key? frame var)
            (hash-set! frame var val)
            (assign-var var val (cdr env))))))

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

(define (no-more-exps? seq) (null? seq))

(define the-global-environment (make-env))

(define (get-global-environment)
  the-global-environment)

(define code
  '(read-eval-print-loop
    ; clear stack
    (assign expr (op read))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (label eval-dispatch))
    print-result
    (perform (op displayln) (reg val))
    (goto (label read-eval-print-loop))

    unknown-expression-type
    (assign val (const "error: unknown expression type"))
    (goto (label signal-error))

    unknown-procedure-type
    (restore continue)
    (assign val (const "error: unknown procedure type"))
    (goto (label signal-error))

    signal-error
    (perform (op displayln) (reg val))
    (goto (label read-eval-print-loop))

    eval-dispatch
    (test (op literal?) (reg expr))
    (branch (label eval-self-eval))
    (test (op symbol?) (reg expr))
    (branch (label eval-variable))
    (test (op quoted?) (reg expr))
    (branch (label eval-quoted))
    (test (op assignment?) (reg expr))
    (branch (label eval-assignment))
    (test (op definition?) (reg expr))
    (branch (label eval-definition))
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
    (assign val (op lookup-var) (reg expr) (reg env))
    (goto (reg continue))
    eval-quoted
    (assign val (op text-of-quotation) (reg expr))
    (goto (reg continue))
    eval-lambda
    (assign unev (op closure-vars) (reg expr))
    (assign expr (op closure-body) (reg expr))
    (assign val (op closure)
            (reg unev) (reg expr) (reg env))
    (goto (reg continue))

    eval-application
    (save continue)
    (save env)
    (assign unev (op operands) (reg expr))
    (save unev)
    (assign expr (op operator) (reg expr))
    (assign continue (label eval-appl-did-operator))
    (goto (label eval-dispatch))
    eval-appl-did-operator
    (restore unev)
    (restore env)
    (assign args (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)
    eval-appl-operand-loop
    (save args)
    (assign expr (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label eval-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label eval-appl-accumulate-arg))
    (goto (label eval-dispatch))
    eval-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore args)
    (assign args (op adjoin-arg) (reg val) (reg args))
    (assign unev (op rest-operands) (reg unev))
    (goto (label eval-appl-operand-loop))
    eval-appl-last-arg
    (assign continue (label eval-appl-accum-last-arg))
    (goto (label eval-dispatch))
    eval-appl-accum-last-arg
    (restore args)
    (assign args (op adjoin-arg) (reg val) (reg args))
    (restore proc)
    (goto (label apply-dispatch))
    apply-dispatch
    (test (op builtin?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))  
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))

    primitive-apply
    (assign val (op builtin-apply)
            (reg proc)
            (reg args))
    (restore continue)
    (goto (reg continue))

    compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op subst)
            (reg unev) (reg args) (reg env))
    (assign unev (op procedure-body) (reg proc))
    (goto (label eval-sequence))

    eval-begin
    (assign unev (op begin-actions) (reg expr))
    (save continue)
    (goto (label eval-sequence))

    eval-sequence
    (assign expr (op first-expr) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label eval-sequence-last-expr))
    (save unev)
    (save env)
    (assign continue (label eval-sequence-continue))
    (goto (label eval-dispatch))
    eval-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label eval-sequence))
    eval-sequence-last-expr
    (restore continue)
    (goto (label eval-dispatch))

    eval-if
    (save expr)
    (save env)
    (save continue)
    (assign continue (label eval-if-decide))
    (assign expr (op if-predicate) (reg expr))
    (goto (label eval-dispatch))
    eval-if-decide
    (restore continue)
    (restore env)
    (restore expr)
    (test (op true?) (reg val))
    (branch (label eval-if-consequent))
    eval-if-alternative
    (assign expr (op if-alternative) (reg expr))
    (goto (label eval-dispatch))
    eval-if-consequent
    (assign expr (op if-consequent) (reg expr))
    (goto (label eval-dispatch))

    eval-assignment
    (assign unev (op assignment-variable) (reg expr))
    (save unev)
    (assign expr (op assignment-value) (reg expr))
    (save env)
    (save continue)
    (assign continue (label eval-assignment-1))
    (goto (label eval-dispatch))
    eval-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform
     (op assign-var) (reg unev) (reg val) (reg env))
    (assign val (const "ok"))
    (goto (reg continue))

    eval-definition
    (assign unev (op definition-variable) (reg expr))
    (save unev)
    (assign expr (op definition-value) (reg expr))
    (save env)
    (save continue)
    (assign continue (label eval-definition-1))
    (goto (label eval-dispatch))
    eval-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform
     (op define-var) (reg unev) (reg val) (reg env))
    (assign val (const "ok"))
    (goto (reg continue))))
