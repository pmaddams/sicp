#lang racket/base

; Exercise 4.42

(provide (all-defined-out))

(require (only-in racket (apply builtin-apply)))

(struct builtin (impl))

(struct closure (vars unev env))

(define (eval expr env)
  ((analyze expr) env))

(define (analyze expr)
  (cond ((literal? expr) (analyze-literal expr))
        ((symbol? expr) (analyze-symbol expr))
        (else (case (car expr)
                ('quote (analyze-quote expr))
                ('lambda (analyze-lambda expr))
                ('define (analyze-define expr))
                ('set! (analyze-set expr))
                ('if (analyze-if expr))
                ('begin (analyze-list (cdr expr)))
                ('cond (analyze (cond->if expr)))
                ('let (analyze (let->lambda expr)))
                (else (analyze-apply expr))))))

(define (apply proc args)
  (if (builtin? proc)
      (builtin-apply (builtin-impl proc) args)
      ((closure-unev proc)
       (subst (closure-vars proc)
              args
              (closure-env proc)))))

(define (literal? expr)
  (or (boolean? expr)
      (number? expr)
      (string? expr)))

(define ((analyze-literal expr) env) expr)

(define ((analyze-symbol expr) env) (lookup-var expr env))

(define ((analyze-quote expr) env) (cadr expr))

(define (analyze-lambda expr)
  (let ((vars (cadr expr))
        (unev (analyze-list (cddr expr))))
    (lambda (env)
      (closure vars unev env))))

(define (analyze-define expr)
  (let ((var (if (symbol? (cadr expr))
                 (cadr expr)
                 (caadr expr)))
        (unev (analyze
               (if (symbol? (cadr expr))
                   (caddr expr)
                   (let ((vars (cdadr expr))
                         (body (cddr expr)))
                     (cons 'lambda (cons vars body)))))))
    (lambda (env)
      (define-var var (unev env) env))))

(define (analyze-set expr)
  (let ((var (cadr expr))
        (unev (analyze (caddr expr))))
    (lambda (env)
      (assign-var var (unev env) env))))

(define (analyze-if expr)
  (let ((unev-predicate (analyze (cadr expr)))
        (unev-consequent (analyze (caddr expr)))
        (unev-alternative (analyze (cadddr expr))))
    (lambda (env)
      (if ((unev-predicate) env)
          ((unev-consequent) env)
          ((unev-alternative) env)))))

(define (analyze-list exprs)
  (define ((sequence u1 u2) env)
    (u1 env)
    (u2 env))

  (let ((analyzed (map analyze exprs)))
    (let loop ((first (car analyzed)) (rest (cdr analyzed)))
      (if (null? rest)
          first
          (loop (sequence first (car rest)) (cdr rest))))))

(define (cond->if expr) (expand (cdr expr)))

(define (expand clauses)
  (if (null? clauses)
      'false
      (let* ((first (car clauses))
             (predicate (car first))
             (actions (cdr first))
             (rest (cdr clauses)))
        (if (eq? predicate 'else)
            (if (null? rest)
                (list->expr actions)
                (error "else clause must be last"))
            (list 'if predicate (list->expr actions) (expand rest))))))

(define (list->expr l)
  (if (null? (cdr l))
      (car l)
      (cons 'begin l)))

(define (let->lambda expr)
  (let* ((bindings (cadr expr))
         (vars (map car bindings))
         (exprs (map cadr bindings))
         (body (cddr expr)))
    (cons (cons 'lambda (cons vars body)) exprs)))

(define (analyze-apply expr)
  (let ((l (map analyze expr)))
    (lambda (env)
      (let* ((vals (map (lambda (u) (u env)) l))
             (proc (car vals))
             (args (cdr vals)))
        (apply proc args)))))

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
  `((cons . ,cons)
    (car . ,car)
    (cdr . ,cdr)
    (pair? . ,pair?)
    (null? . ,null?)
    (+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)
    (< . ,<)
    (> . ,>)
    (= . ,=)
    (eq? . ,eq?)
    (display . ,display)
    (newline . ,newline)))

(define (make-env)
  (let ((vars (map car builtins))
        (vals (map builtin (map cdr builtins))))
    (subst vars vals '())))

(define (interpret code)
  (let ((env (make-env)))
    (for ((expr code))
      (let ((val (eval expr env)))
        (unless (void? val)
          (displayln val))))))
