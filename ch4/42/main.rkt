#lang racket/base

; Exercise 4.42

(provide (all-defined-out))

(require racket/function
         (only-in racket (apply builtin-apply)))

(struct builtin (impl))

(struct closure (vars unev env))

(define (eval expr env succeed fail)
  ((analyze expr) env succeed fail))

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
                ('amb (analyze-amb expr))
                (else (analyze-apply expr))))))

(define (apply proc args succeed fail)
  (if (builtin? proc)
      (succeed (builtin-apply (builtin-impl proc) args) fail)
      ((closure-unev proc)
       (subst (closure-vars proc)
              args
              (closure-env proc))
       succeed
       fail)))

(define (literal? expr)
  (or (boolean? expr)
      (number? expr)
      (string? expr)))

(define ((analyze-literal expr) env succeed fail)
  (succeed expr fail))

(define ((analyze-symbol expr) env succeed fail)
  (succeed (lookup-var expr env) fail))

(define ((analyze-quote expr) env succeed fail)
  (succeed (cadr expr) fail))

(define (analyze-lambda expr)
  (let ((vars (cadr expr))
        (unev (analyze-list (cddr expr))))
    (lambda (env succeed fail)
      (succeed (closure vars unev env) fail))))

(define (analyze-define expr)
  (let* ((var (if (symbol? (cadr expr))
                  (cadr expr)
                  (caadr expr)))
         (x (if (symbol? (cadr expr))
                (caddr expr)
                (let ((vars (cdadr expr))
                      (body (cddr expr)))
                  (cons 'lambda (cons vars body)))))
         (unev (analyze x)))
    (lambda (env succeed fail)
      (unev
       env
       (lambda (val fail*)
         (succeed (define-var var val env) fail*))
       fail))))

(define (analyze-set expr)
  (let* ((var (cadr expr))
         (x (caddr expr))
         (unev (analyze x)))
    (lambda (env succeed fail)
      (unev
       env
       (lambda (val fail*)
         (let ((old-val (lookup-var var env)))
           (succeed (assign-var var val env)
                    (thunk (assign-var var old-val env)
                           (fail*)))))
       fail))))

(define (analyze-if expr)
  (let ((unev-predicate (analyze (cadr expr)))
        (unev-consequent (analyze (caddr expr)))
        (unev-alternative (analyze (cadddr expr))))
    (lambda (env succeed fail)
      (unev-predicate
       env
       (lambda (val fail*)
         (if val
             (unev-consequent env succeed fail*)
             (unev-alternative env succeed fail*)))
       fail))))

(define (analyze-list exprs)
  (define ((sequence u1 u2) env succeed fail)
    (u1
     env
     (lambda (val fail*)
       (u2 env succeed fail*))
     fail))

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

(define (analyze-amb expr)
  (let ((unev-choices (analyze (cdr expr))))
    (lambda (env succeed fail)
      (let loop ((l unev-choices))
        (if (null? l)
            (fail)
            ((car l)
             env
             succeed
             (loop (cdr l))))))))

(define (analyze-apply expr)
  (let ((unev-proc (analyze (car expr)))
        (unev-args (map analyze (cdr expr))))
    (lambda (env succeed fail)
      (unev-proc
       env
       (lambda (proc fail*)
         (get-args unev-args
                   env
                   (lambda (args fail**)
                     (apply proc args succeed fail**))
                   fail*))
       fail))))

(define (get-args unev-args env succeed fail)
  (if (null? unev-args)
      (succeed '() fail)
      ((car unev-args)
       env
       (lambda (arg fail*)
         (get-args (cdr unev-args)
                   env
                   (lambda (args fail**)
                     (succeed (cons arg args) fail**))
                   fail*))
       fail)))

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
    (let loop ((next void))
      (unless (null? code)
        (let ((expr (car code)))
          (set! code (cdr code))
          (if (eq? expr 'next)
              (next)
              (eval expr
                    env
                    (lambda (val next*)
                      (unless (void? val)
                        (displayln val))
                      (loop next*))
                    void)))))))
