#lang racket/base

; Exercise 4.42

(provide (all-defined-out))

(require (only-in racket (apply apply*))
         racket/function
         lisp/env)

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

(define (interpret code)
  (let ((env (make-env))
        (start void))
    (let loop ((val #f) (fail start))
      (if (null? code)
          val
          (let ((expr (car code)))
            (set! code (cdr code))
            (if (eq? expr 'next)
                (fail)
                (eval expr
                      env
                      (lambda (val fail*)
                        (loop val fail*))
                      (thunk (loop start)))))))))

(struct closure (vars proc env))

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

(define (apply op args succeed fail)
  (if (procedure? op)
      (succeed (apply* op args) fail)
      ((closure-proc op)
       (subst (closure-vars op)
              args
              (closure-env op))
       succeed
       fail)))

(define (literal? expr)
  (or (boolean? expr)
      (number? expr)
      (string? expr)))

(define ((analyze-literal expr) env succeed fail)
  (succeed expr fail))

(define ((analyze-symbol expr) env succeed fail)
  (succeed (get-var expr env) fail))

(define ((analyze-quote expr) env succeed fail)
  (succeed (cadr expr) fail))

(define (analyze-lambda expr)
  (let ((vars (cadr expr))
        (proc (analyze-list (cddr expr))))
    (lambda (env succeed fail)
      (succeed (closure vars proc env) fail))))

(define (analyze-define expr)
  (let* ((var (if (symbol? (cadr expr))
                  (cadr expr)
                  (caadr expr)))
         (x (if (symbol? (cadr expr))
                (caddr expr)
                (let ((vars (cdadr expr))
                      (body (cddr expr)))
                  (cons 'lambda (cons vars body)))))
         (proc (analyze x)))
    (lambda (env succeed fail)
      (proc
       env
       (lambda (val fail*)
         (succeed (define-var var val env) fail*))
       fail))))

(define (analyze-set expr)
  (let* ((var (cadr expr))
         (x (caddr expr))
         (proc (analyze x)))
    (lambda (env succeed fail)
      (proc
       env
       (lambda (val fail*)
         (let ((old-val (get-var var env)))
           (succeed (set-var var val env)
                    (thunk (set-var var old-val env)
                           (fail*)))))
       fail))))

(define (analyze-if expr)
  (let ((predicate-proc (analyze (cadr expr)))
        (consequent-proc (analyze (caddr expr)))
        (alternative-proc (analyze (cadddr expr))))
    (lambda (env succeed fail)
      (predicate-proc
       env
       (lambda (val fail*)
         (if val
             (consequent-proc env succeed fail*)
             (alternative-proc env succeed fail*)))
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

(define (analyze-amb expr)
  (let ((choice-procs (map analyze (cdr expr))))
    (lambda (env succeed fail)
      (let loop ((procs choice-procs))
        (if (null? procs)
            (fail)
            ((car procs)
             env
             succeed
             (thunk (loop (cdr procs)))))))))

(define (analyze-apply expr)
  (let ((op-proc (analyze (car expr)))
        (arg-procs (map analyze (cdr expr))))
    (lambda (env succeed fail)
      (op-proc
       env
       (lambda (op fail*)
         (get-args arg-procs
                   env
                   (lambda (args fail**)
                     (apply op args succeed fail**))
                   fail*))
       fail))))

(define (get-args arg-procs env succeed fail)
  (if (null? arg-procs)
      (succeed '() fail)
      ((car arg-procs)
       env
       (lambda (arg fail*)
         (get-args (cdr arg-procs)
                   env
                   (lambda (args fail**)
                     (succeed (cons arg args) fail**))
                   fail*))
       fail)))

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
