#lang racket/base

; Exercise 5.23

(provide (all-defined-out))

(require racket/class
         vm)

(define (execute code)
  (let ((vm (make-vm lisp)))
    (set! env (make-env))
    (set! buf code)
    (send vm execute)
    (send vm get 'val)))

(define lisp
  '(loop
    (assign expr (op input))
    (test (op eof-object?) (reg expr))
    (branch (label done))
    (assign env (op get-env))
    (assign continue (label loop))
    eval
    (test (op literal?) (reg expr))
    (branch (label eval-literal))
    (test (op symbol?) (reg expr))
    (branch (label eval-variable))
    (test (op quote-expr?) (reg expr))
    (branch (label eval-quote))
    (test (op lambda-expr?) (reg expr))
    (branch (label eval-lambda))
    (test (op define-expr?) (reg expr))
    (branch (label eval-define))
    (test (op set-expr?) (reg expr))
    (branch (label eval-set))
    (test (op if-expr?) (reg expr))
    (branch (label eval-if))
    (test (op begin-expr?) (reg expr))
    (branch (label eval-begin))
    (test (op cond-expr?) (reg expr))
    (branch (label eval-cond))
    (test (op and-expr?) (reg expr))
    (branch (label eval-and))
    (test (op or-expr?) (reg expr))
    (branch (label eval-or))
    (test (op let-expr?) (reg expr))
    (branch (label eval-let))
    (test (op pair?) (reg expr))
    (branch (label eval-application))
    (perform (op error) (const "syntax error"))
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
    (assign unev (op lambda-expr-vars) (reg expr))
    (assign expr (op lambda-expr-body) (reg expr))
    (assign val (op closure) (reg unev) (reg expr) (reg env))
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
    (goto (reg continue))
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
    eval-if
    (save expr)
    (save env)
    (save continue)
    (assign continue (label eval-if-1))
    (assign expr (op if-expr-predicate) (reg expr))
    (goto (label eval))
    eval-if-1
    (restore continue)
    (restore env)
    (restore expr)
    (test (op true?) (reg val))
    (branch (label eval-if-consequent))
    (assign expr (op if-expr-alternative) (reg expr))
    (goto (label eval))
    eval-if-consequent
    (assign expr (op if-expr-consequent) (reg expr))
    (goto (label eval))
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
    eval-cond
    (assign expr (op cond->if) (reg expr))
    (goto (label eval))
    eval-and
    (assign expr (op and->if) (reg expr))
    (goto (label eval))
    eval-or
    (assign expr (op or->if) (reg expr))
    (goto (label eval))
    eval-let
    (assign expr (op let->lambda) (reg expr))
    (goto (label eval))
    eval-application
    (save continue)
    (save env)
    (assign unev (op cdr) (reg expr))
    (save unev)
    (assign expr (op car) (reg expr))
    (assign continue (label eval-application-after-operator))
    (goto (label eval))
    eval-application-after-operator
    (restore unev)
    (restore env)
    (assign args (op empty-list))
    (assign proc (reg val))
    (test (op null?) (reg unev))
    (branch (label apply))
    (save proc)
    eval-application-loop
    (save args)
    (assign expr (op car) (reg unev))
    (test (op singleton?) (reg unev))
    (branch (label eval-application-after-loop))
    (save env)
    (save unev)
    (assign continue (label eval-application-accumulate))
    (goto (label eval))
    eval-application-accumulate
    (restore unev)
    (restore env)
    (restore args)
    (assign args (op adjoin) (reg val) (reg args))
    (assign unev (op cdr) (reg unev))
    (goto (label eval-application-loop))
    eval-application-after-loop
    (assign continue (label eval-application-accumulate-last))
    (goto (label eval))
    eval-application-accumulate-last
    (restore args)
    (assign args (op adjoin) (reg val) (reg args))
    (restore proc)
    (goto (label apply))
    apply
    (test (op procedure?) (reg proc))
    (branch (label apply-builtin))
    (goto (label apply-closure))
    apply-builtin
    (assign val (op apply) (reg proc) (reg args))
    (restore continue)
    (goto (reg continue))
    apply-closure
    (assign unev (op closure-vars) (reg proc))
    (assign env (op closure-env) (reg proc))
    (assign env (op subst) (reg unev) (reg args) (reg env))
    (assign unev (op closure-body) (reg proc))
    (goto (label eval-list))
    done))

(struct closure (vars body env))

(define (literal? expr)
  (or (boolean? expr)
      (number? expr)
      (string? expr)))

(define (quote-expr? expr)
  (type? 'quote expr))

(define (lambda-expr? expr)
  (type? 'lambda expr))

(define lambda-expr-vars cadr)

(define lambda-expr-body cddr)

(define (define-expr? expr)
  (type? 'define expr))

(define (define-expr-var expr)
  (if (symbol? (cadr expr))
      (cadr expr)
      (caadr expr)))

(define (define-expr-val expr)
  (if (symbol? (cadr expr))
      (caddr expr)
      (cons 'lambda
            (cons (cdadr expr)
                  (cddr expr)))))

(define (set-expr? expr)
  (type? 'set! expr))

(define set-expr-var cadr)

(define set-expr-val caddr)

(define (if-expr? expr)
  (type? 'if expr))

(define if-expr-predicate cadr)

(define if-expr-consequent caddr)

(define if-expr-alternative cadddr)

(define (begin-expr? expr)
  (type? 'begin expr))

(define (cond-expr? expr)
  (type? 'cond expr))

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

(define (and-expr? expr)
  (type? 'and expr))

(define (and->if expr) (expand-and (cdr expr)))

(define (expand-and exprs)
  (cond ((null? exprs) #t)
        ((null? (cdr exprs)) (car exprs))
        (else (let ((predicate (car exprs))
                    (consequent (expand-and (cdr exprs)))
                    (alternative #f))
                (list 'if predicate consequent alternative)))))

(define (or-expr? expr)
  (type? 'or expr))

(define (or->if expr) (expand-or (cdr expr)))

(define (expand-or exprs)
  (cond ((null? exprs) #f)
        ((null? (cdr exprs)) (car exprs))
        (else (let ((predicate (car exprs))
                    (consequent (car exprs))
                    (alternative (expand-or (cdr exprs))))
                (list 'if predicate consequent alternative)))))

(define (let-expr? expr)
  (type? 'let expr))

(define (let->lambda expr)
  (let* ((bindings (cadr expr))
         (vars (map car bindings))
         (exprs (map cadr bindings))
         (body (cddr expr)))
    (cons (cons 'lambda (cons vars body)) exprs)))

(define (type? t x)
  (and (pair? x)
       (eq? t (car x))))

(define (singleton? l) (null? (cdr l)))

(define (adjoin x l)
  (append l (list x)))

(define (empty-list) '())

(define (true? x) (not (false? x)))

(define (false? x) (eq? x #f))

(define (subst vars vals env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) env)
      (error "arity mismatch:" vars vals)))

(define (make-frame vars vals)
  (make-hash (map cons vars vals)))

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
        (vals (map cdr builtins)))
    (subst vars vals '())))

(define env '())

(define (get-env) env)

(define buf '())

(define (input)
  (if (null? buf)
      eof
      (let ((expr (car buf)))
        (set! buf (cdr buf))
        expr)))
