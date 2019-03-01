#lang racket/base

; Exercise 4.34

(provide (all-defined-out))

(require (only-in racket (apply builtin-apply)))

(struct builtin (impl))

(struct closure (vars body env))

(struct promise (forced val expr env) #:mutable)

(struct stream (proc))

(define (eval expr env)
  (cond ((literal? expr) expr)
        ((symbol? expr) (lookup-var expr env))
        (else (case (car expr)
                ('quote (cadr expr))
                ('lambda (let ((vars (cadr expr))
                               (body (cddr expr)))
                           (closure vars body env)))
                ('define (eval-define expr env))
                ('set! (eval-set expr env))
                ('if (eval-if expr env))
                ('begin (eval-list (cdr expr) env))
                ('cond (eval-if (cond->if expr) env))
                (else (let ((proc (value (car expr) env))
                            (args (cdr expr)))
                        (apply proc args env)))))))

(define (apply proc args env)
  (if (builtin? proc)
      (let ((vals (map (lambda (x) (value x env)) args)))
        (builtin-apply (builtin-impl proc) vals))
      (let ((promises (map (lambda (x) (delay x env)) args)))
        (eval-list (closure-body proc)
                   (subst (closure-vars proc)
                          promises
                          (closure-env proc))))))

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

(define (eval-define expr env)
  (let ((var (if (symbol? (cadr expr))
                 (cadr expr)
                 (caadr expr)))
        (x (if (symbol? (cadr expr))
               (caddr expr)
               (let ((vars (cdadr expr))
                     (body (cddr expr)))
                 (cons 'lambda (cons vars body))))))
    (define-var var (eval x env) env)))

(define (eval-set expr env)
  (let ((var (cadr expr))
        (x (caddr expr)))
    (assign-var var (eval x env) env)))

(define (eval-if expr env)
  (let ((predicate (cadr expr))
        (consequent (caddr expr))
        (alternative (cadddr expr)))
    (if (value predicate env)
        (eval consequent env)
        (eval alternative env))))

(define (eval-list l env)
  (let ((obj (eval (car l) env)))
    (if (null? (cdr l))
        obj
        (eval-list (cdr l) env))))

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

(define (subst vars objs env)
  (cons (make-frame vars objs) env))

(define (make-frame vars objs)
  (if (not (= (length vars) (length objs)))
      (error "arity mismatch:" vars objs)
      (let ((assocs (map cons vars objs)))
        (make-hash assocs))))

(define (define-var var obj env)
  (let ((frame (car env)))
    (hash-set! frame var obj)))

(define (lookup-var var env)
  (if (null? env)
      (error "undefined:" var)
      (let ((frame (car env)))
        (if (hash-has-key? frame var)
            (hash-ref frame var)
            (lookup-var var (cdr env))))))

(define (assign-var var obj env)
  (if (null? env)
      (error "undefined:" var)
      (let ((frame (car env)))
        (if (hash-has-key? frame var)
            (hash-set! frame var obj)
            (assign-var var obj (cdr env))))))

(define (stream-cons x y)
  (stream
   (lambda (m)
     (m x y))))

(define (stream-first s)
  ((stream-proc s)
   (lambda (x y) x)))

(define (stream-rest s)
  ((stream-proc s)
   (lambda (x y) y)))

(define (stream-display x)
  (display
   (if (stream? x)
       (format "(~a ..." (stream-first x))
       x)))

(define builtins
  `((cons . ,stream-cons)
    (car . ,stream-first)
    (cdr . ,stream-rest)
    (pair? . ,stream?)
    (null? . ,null?)
    (+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)
    (< . ,<)
    (> . ,>)
    (= . ,=)
    (eq? . ,eq?)
    (display . ,stream-display)
    (newline . ,newline)))

(define (make-env)
  (let ((vars (map car builtins))
        (vals (map builtin (map cdr builtins))))
    (subst vars vals '())))

(define (interpret code)
  (let ((env (make-env)))
    (for ((expr code))
      (let ((val (value expr env)))
        (unless (void? val)
          (displayln val))))))
