#lang racket/base

; Exercise 4.42

(provide (all-defined-out))

(require racket/function
         (only-in racket (apply builtin-apply)))

(struct builtin (impl))

(struct closure (vars exec env))

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
                ('and (analyze (and->if expr)))
                ('or (analyze (or->if expr)))
                ('let (analyze (let->lambda expr)))
                ('amb (analyze-amb expr))
                (else (analyze-apply expr))))))

(define (apply proc args succeed fail)
  (if (builtin? proc)
      (succeed (builtin-apply (builtin-impl proc) args) fail)
      ((closure-exec proc)
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
        (exec (analyze-list (cddr expr))))
    (lambda (env succeed fail)
      (succeed (closure vars exec env) fail))))

(define (analyze-define expr)
  (let* ((var (if (symbol? (cadr expr))
                  (cadr expr)
                  (caadr expr)))
         (x (if (symbol? (cadr expr))
                (caddr expr)
                (let ((vars (cdadr expr))
                      (body (cddr expr)))
                  (cons 'lambda (cons vars body)))))
         (exec (analyze x)))
    (lambda (env succeed fail)
      (exec
       env
       (lambda (val fail*)
         (succeed (define-var var val env) fail*))
       fail))))

(define (analyze-set expr)
  (let* ((var (cadr expr))
         (x (caddr expr))
         (exec (analyze x)))
    (lambda (env succeed fail)
      (exec
       env
       (lambda (val fail*)
         (let ((old-val (lookup-var var env)))
           (succeed (assign-var var val env)
                    (thunk (assign-var var old-val env)
                           (fail*)))))
       fail))))

(define (analyze-if expr)
  (let ((predicate-exec (analyze (cadr expr)))
        (consequent-exec (analyze (caddr expr)))
        (alternative-exec (analyze (cadddr expr))))
    (lambda (env succeed fail)
      (predicate-exec
       env
       (lambda (val fail*)
         (if val
             (consequent-exec env succeed fail*)
             (alternative-exec env succeed fail*)))
       fail))))

(define (analyze-list exprs)
  (define ((sequence e1 e2) env succeed fail)
    (e1
     env
     (lambda (val fail*)
       (e2 env succeed fail*))
     fail))

  (let ((analyzed (map analyze exprs)))
    (let loop ((first (car analyzed)) (rest (cdr analyzed)))
      (if (null? rest)
          first
          (loop (sequence first (car rest)) (cdr rest))))))

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
  (if (null? exprs)
      #t
      (let loop ((exprs exprs))
        (if (null? (cdr exprs))
            (car exprs)
            (let ((predicate (car exprs))
                  (consequent (loop (cdr exprs)))
                  (alternative #f))
              (list 'if predicate consequent alternative))))))

(define (or->if expr) (expand-or (cdr expr)))

(define (expand-or exprs)
  (if (null? exprs)
      #f
      (let loop ((exprs exprs))
        (if (null? (cdr exprs))
            (car exprs)
            (let ((predicate (car exprs))
                  (consequent (car exprs))
                  (alternative (loop (cdr exprs))))
              (list 'if predicate consequent alternative))))))

(define (let->lambda expr)
  (let* ((bindings (cadr expr))
         (vars (map car bindings))
         (exprs (map cadr bindings))
         (body (cddr expr)))
    (cons (cons 'lambda (cons vars body)) exprs)))

(define (analyze-amb expr)
  (let ((choice-execs (map analyze (cdr expr))))
    (lambda (env succeed fail)
      (let loop ((execs choice-execs))
        (if (null? execs)
            (fail)
            ((car execs)
             env
             succeed
             (thunk (loop (cdr execs)))))))))

(define (analyze-apply expr)
  (let ((proc-exec (analyze (car expr)))
        (arg-execs (map analyze (cdr expr))))
    (lambda (env succeed fail)
      (proc-exec
       env
       (lambda (proc fail*)
         (get-args arg-execs
                   env
                   (lambda (args fail**)
                     (apply proc args succeed fail**))
                   fail*))
       fail))))

(define (get-args arg-execs env succeed fail)
  (if (null? arg-execs)
      (succeed '() fail)
      ((car arg-execs)
       env
       (lambda (arg fail*)
         (get-args (cdr arg-execs)
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

(define (interpret code)
  (let ((env (make-env))
        (start void))
    (let loop ((fail start))
      (unless (null? code)
        (let ((expr (car code)))
          (set! code (cdr code))
          (if (eq? expr 'next)
              (fail)
              (eval expr
                    env
                    (lambda (val fail*)
                      (unless (void? val)
                        (displayln val))
                      (loop fail*))
                    (thunk (loop start)))))))))

(define liars
  '((define (require p) (or p (amb)))

    (define (distinct? l)
      (or (null? l)
          (and (not (memq (car l) (cdr l)))
               (distinct? (cdr l)))))

    (define (memq x l)
      (and (not (null? l))
           (if (eq? x (car l))
               l
               (memq x (cdr l)))))

    (let ((betty (amb 1 2 3 4 5))
          (ethel (amb 1 2 3 4 5))
          (joan (amb 1 2 3 4 5))
          (kitty (amb 1 2 3 4 5))
          (mary (amb 1 2 3 4 5)))
      (require (distinct? (list betty ethel joan kitty mary)))
      (require (or (= kitty 2) (= betty 3)))
      (require (or (= ethel 1) (= joan 2)))
      (require (or (= joan 3) (= ethel 5)))
      (require (or (= kitty 2) (= mary 4)))
      (require (or (= mary 4) (= betty 1)))
      (list (list 'betty betty)
            (list 'ethel ethel)
            (list 'joan joan)
            (list 'kitty kitty)
            (list 'mary mary)))))
