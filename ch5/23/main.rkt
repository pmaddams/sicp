#lang racket/base

; Exercise 5.23

(provide (all-defined-out))

(require "../../vm/vm.rkt")

(require (only-in racket (apply apply-builtin)))

(define interpreter
  '(loop
    (assign expr (op read))
    (assign env (op get-global-env))
    (assign continue (label print))
    (goto (label eval))
    print
    (perform (op displayln) (reg val))
    (goto (label loop))
    unknown-expression-type
    (assign val (const "error: unknown expression type"))
    (goto (label error))
    unknown-procedure-type
    (restore continue)
    (assign val (const "error: unknown procedure type"))
    (goto (label error))
    error
    (perform (op displayln) (reg val))
    (goto (label loop))
    eval
    (test (op literal?) (reg expr))
    (branch (label eval-literal))
    (test (op symbol?) (reg expr))
    (branch (label eval-variable))
    (test (op quote?) (reg expr))
    (branch (label eval-quote))
    (test (op lambda?) (reg expr))
    (branch (label eval-lambda))
    (test (op define?) (reg expr))
    (branch (label eval-define))
    (test (op set?) (reg expr))
    (branch (label eval-set))
    (test (op if?) (reg expr))
    (branch (label eval-if))
    (test (op begin?) (reg expr))
    (branch (label eval-begin))
    (test (op pair?) (reg expr))
    (branch (label eval-application))
    (goto (label unknown-expression-type))
    eval-literal
    (assign val (reg expr))
    (goto (reg continue))
    eval-variable
    (assign val (op get-var) (reg expr) (reg env))
    (goto (reg continue))
    eval-quote
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
    (assign unev (op cdr) (reg expr))
    (save unev)
    (assign expr (op car) (reg expr))
    (assign continue (label eval-appl-did-operator))
    (goto (label eval))
    eval-appl-did-operator
    (restore unev)
    (restore env)
    (assign args (op empty-list))
    (assign proc (reg val))
    (test (op null?) (reg unev))
    (branch (label apply))
    (save proc)
    eval-appl-operand-loop
    (save args)
    (assign expr (op car) (reg unev))
    (test (op singleton?) (reg unev))
    (branch (label eval-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label eval-appl-accumulate-arg))
    (goto (label eval))
    eval-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore args)
    (assign args (op adjoin) (reg val) (reg args))
    (assign unev (op cdr) (reg unev))
    (goto (label eval-appl-operand-loop))
    eval-appl-last-arg
    (assign continue (label eval-appl-accum-last-arg))
    (goto (label eval))
    eval-appl-accum-last-arg
    (restore args)
    (assign args (op adjoin) (reg val) (reg args))
    (restore proc)
    (goto (label apply))
    apply
    (test (op builtin?) (reg proc))
    (branch (label apply-builtin))
    (test (op closure?) (reg proc))
    (branch (label apply-closure))
    (goto (label unknown-procedure-type))
    apply-builtin
    (assign val (op apply-builtin) (reg proc) (reg args))
    (restore continue)
    (goto (reg continue))
    apply-closure
    (assign unev (op closure-vars) (reg proc))
    (assign env (op closure-env) (reg proc))
    (assign env (op subst) (reg unev) (reg args) (reg env))
    (assign unev (op closure-body) (reg proc))
    (goto (label eval-list))
    eval-begin
    (assign unev (op cdr) (reg expr))
    (save continue)
    (goto (label eval-list))
    eval-list
    (assign expr (op car) (reg unev))
    (test (op singleton?) (reg unev))
    (branch (label eval-list-last))
    (save unev)
    (save env)
    (assign continue (label eval-list-continue))
    (goto (label eval))
    eval-list-continue
    (restore env)
    (restore unev)
    (assign unev (op cdr) (reg unev))
    (goto (label eval-list))
    eval-list-last
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
    (assign val (const ""))
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
    (assign val (const ""))
    (goto (reg continue))))

(struct builtin (proc))

(struct closure (vars body env))

(define (literal? expr)
  (or (boolean? expr)
      (number? expr)
      (string? expr)))

(define (quote? expr)
  (tagged-list? expr 'quote))

(define (lambda? expr)
  (tagged-list? expr 'lambda))

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

(define (set? expr)
  (tagged-list? expr 'set!))

(define set-expr-var cadr)

(define set-expr-val caddr)

(define (if? expr)
  (tagged-list? expr 'if))

(define if-expr-predicate cadr)

(define if-expr-consequent caddr)

(define if-expr-alternative cadddr)

(define (true? x) (not (false? x)))

(define (false? x) (eq? x #f))

(define (begin? expr)
  (tagged-list? expr 'begin))

(define (empty-list) '())

(define (adjoin x l)
  (append l (list x)))

(define (singleton? l) (null? (cdr l)))

(define (tagged-list? x tag)
  (and (pair? x)
       (eq? tag (car x))))

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

(define global-env (make-env))

(define (get-global-env) global-env)
