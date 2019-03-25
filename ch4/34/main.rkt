#lang racket/base

; Exercise 4.34

(provide (all-defined-out))

(require (only-in racket (apply apply*))
         lisp/env)

(define (interpret code)
  (let ((env (make-env)))
    (for/last ((expr (in-list code)))
      (value expr env))))

(struct closure (vars body env))

(struct promise (forced val expr env) #:mutable)

(struct stream (proc))

(define (eval expr env)
  (cond ((literal? expr) expr)
        ((symbol? expr) (get-var expr env))
        (else (case (car expr)
                ('quote (cadr expr))
                ('lambda (eval-lambda expr env))
                ('define (eval-define expr env))
                ('if (eval-if expr env))
                ('begin (eval-list (cdr expr) env))
                ('cond (eval (cond->if expr) env))
                ('and (eval (and->if expr) env))
                ('or (eval (or->if expr) env))
                ('let (eval (let->lambda expr) env))
                (else (let ((op (value (car expr) env))
                            (args (cdr expr)))
                        (apply op args env)))))))

(define (apply op args env)
  (if (procedure? op)
      (let ((vals (map (lambda (x) (value x env)) args)))
        (apply* op vals))
      (let ((promises (map (lambda (x) (delay x env)) args)))
        (eval-list (closure-body op)
                   (subst (closure-vars op)
                          promises
                          (closure-env op))))))

(define (value expr env)
  (force (eval expr env)))

(define (force obj)
  (if (promise? obj)
      (if (promise-forced obj)
          (promise-val obj)
          (let ((val (value (promise-expr obj) (promise-env obj))))
            (set-promise-forced! obj #t)
            (set-promise-val! obj val)
            (set-promise-expr! obj (void))
            (set-promise-env! obj (void))
            val))
      obj))

(define (delay expr env)
  (promise #f (void) expr env))

(define (literal? expr)
  (or (boolean? expr)
      (number? expr)
      (string? expr)))

(define (eval-lambda expr env)
  (let ((vars (cadr expr))
        (body (cddr expr)))
    (closure vars body env)))

(define (eval-define expr env)
  (let* ((var (if (symbol? (cadr expr))
                  (cadr expr)
                  (caadr expr)))
         (x (if (symbol? (cadr expr))
                (caddr expr)
                (let ((vars (cdadr expr))
                      (body (cddr expr)))
                  (cons 'lambda (cons vars body)))))
         (obj (eval x env)))
    (define-var var obj env)))

(define (eval-if expr env)
  (let ((predicate (cadr expr))
        (consequent (caddr expr))
        (alternative (cadddr expr)))
    (if (value predicate env)
        (eval consequent env)
        (eval alternative env))))

(define (eval-list exprs env)
  (let ((val (eval (car exprs) env)))
    (if (null? (cdr exprs))
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

(define (stream-cons x y)
  (stream
   (lambda (m)
     (m x y))))

(define (stream-first st)
  ((stream-proc st)
   (lambda (x y) x)))

(define (stream-rest st)
  ((stream-proc st)
   (lambda (x y) y)))

(define (stream-display x)
  (display
   (if (stream? x)
       (format "(~a ..." (stream-first x))
       x)))

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
    (pair? . ,stream?)
    (cons . ,stream-cons)
    (car . ,stream-first)
    (cdr . ,stream-rest)
    (display . ,stream-display)
    (newline . ,newline)))

(define (make-env)
  (let ((vars (map car builtins))
        (vals (map cdr builtins)))
    (subst vars vals '())))
