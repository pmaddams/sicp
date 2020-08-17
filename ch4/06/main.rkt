#lang racket/base

; Exercise 4.6

(provide (all-defined-out))

(require (only-in racket
                  (procedure? builtin?)
                  (apply apply*))
         lisp/env)

(define (interpret code)
  (let ((env (make-env builtins)))
    (for/last ((expr (in-list code)))
      (eval expr env))))

(struct closure (params body env))

(define (eval expr env)
  (cond ((literal? expr) expr)
        ((variable? expr) (get-var expr env))
        (else (case (car expr)
                ('quote (cadr expr))
                ('lambda (eval-lambda expr env))
                ('define (eval-define expr env))
                ('set! (eval-set expr env))
                ('if (eval-if expr env))
                ('begin (eval-list (cdr expr) env))
                ('cond (eval (cond->if expr) env))
                ('and (eval (and->if expr) env))
                ('or (eval (or->if expr) env))
                ('let (eval (let->lambda expr) env))
                (else (let* ((vals (map (lambda (x) (eval x env)) expr))
                             (op (car vals))
                             (args (cdr vals)))
                        (apply op args)))))))

(define (apply op args)
  (if (builtin? op)
      (apply* op args)
      (eval-list (closure-body op)
                 (subst (closure-params op)
                        args
                        (closure-env op)))))

(define (literal? expr)
  (or (boolean? expr)
      (number? expr)))

(define variable? symbol?)

(define (eval-lambda expr env)
  (let ((params (cadr expr))
        (body (cddr expr)))
    (closure params body env)))

(define (eval-define expr env)
  (let* ((var (if (symbol? (cadr expr))
                  (cadr expr)
                  (caadr expr)))
         (x (if (symbol? (cadr expr))
                (caddr expr)
                (let ((params (cdadr expr))
                      (body (cddr expr)))
                  (cons 'lambda (cons params body)))))
         (val (eval x env)))
    (define-var var val env)))

(define (eval-set expr env)
  (let* ((var (cadr expr))
         (x (caddr expr))
         (val (eval x env)))
    (set-var var val env)))

(define (eval-if expr env)
  (let ((predicate (cadr expr))
        (consequent (caddr expr))
        (alternative (cadddr expr)))
    (if (eval predicate env)
        (eval consequent env)
        (eval alternative env))))

(define (eval-list exprs env)
  (let ((val (eval (car exprs) env)))
    (if (last? exprs)
        val
        (eval-list (cdr exprs) env))))

(define (cond->if expr) (expand-cond (cdr expr)))

(define (expand-cond clauses)
  (if (null? clauses)
      #f
      (let* ((first (car clauses))
             (predicate (car first))
             (consequent (list->expr (cdr first)))
             (rest (cdr clauses)))
        (if (eq? predicate 'else)
            consequent
            (let ((alternative (expand-cond rest)))
              (list 'if predicate consequent alternative))))))

(define (list->expr exprs)
  (if (last? exprs)
      (car exprs)
      (cons 'begin exprs)))

(define (and->if expr) (expand-and (cdr expr)))

(define (expand-and exprs)
  (cond ((null? exprs) #t)
        ((last? exprs) (car exprs))
        (else (let ((predicate (car exprs))
                    (consequent (expand-and (cdr exprs)))
                    (alternative #f))
                (list 'if predicate consequent alternative)))))

(define (or->if expr) (expand-or (cdr expr)))

(define (expand-or exprs)
  (cond ((null? exprs) #f)
        ((last? exprs) (car exprs))
        (else (let ((predicate (car exprs))
                    (consequent (car exprs))
                    (alternative (expand-or (cdr exprs))))
                (list 'if predicate consequent alternative)))))

(define (let->lambda expr)
  (let* ((bindings (cadr expr))
         (params (map car bindings))
         (exprs (map cadr bindings))
         (body (cddr expr)))
    (cons (cons 'lambda (cons params body)) exprs)))

(define (last? l) (null? (cdr l)))

(define builtins
  `(; types
    (null? . ,null?)
    (pair? . ,pair?)
    (symbol? . ,symbol?)
    (boolean? . ,boolean?)
    (number? . ,number?)
    (builtin? . ,builtin?)

    ; arithmetic
    (+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)

    ; logic
    (< . ,<)
    (> . ,>)
    (= . ,=)
    (eq? . ,eq?)
    (not . ,not)

    ; data structures
    (cons . ,cons)
    (car . ,car)
    (cdr . ,cdr)
    (list . ,list)

    ; input/output
    (display . ,display)
    (newline . ,newline)
    (read . ,read)
    (eof-object? . ,eof-object?)

    ; environment
    (make-env . ,make-env)
    (subst . ,subst)
    (define-var . ,define-var)
    (get-var . ,get-var)
    (set-var . ,set-var)

    ; application
    (apply* . ,apply*)))
