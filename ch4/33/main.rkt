#lang racket/base

; Exercise 4.34

(provide (all-defined-out))

(require (only-in racket (apply apply*))
         lisp/env)

(define ramanujan
  '((define (merge-weighted l1 l2 w)
      (let ((a (car l1))
            (b (car l2)))
        (if (< (w a) (w b))
            (cons a (merge-weighted (cdr l1) l2 w))
            (cons b (merge-weighted l1 (cdr l2) w)))))

    (define (weighted-pairs l1 l2 w)
      (let ((a (car l1))
            (b (car l2)))
        (cons (cons a b)
              (merge-weighted
               (map (lambda (b*) (cons a b*)) (cdr l2))
               (weighted-pairs (cdr l1) (cdr l2) w)
               w))))

    (define (map f l)
      (if (null? l)
          '()
          (cons (f (car l)) (map f (cdr l)))))

    (define (sum-of-cubes p)
      (+ (cube (car p))
         (cube (cdr p))))

    (define (cube n) (* n n n))

    (define (naturals n)
      (cons n (naturals (+ n 1))))

    (define (loop l)
      (let ((n (sum-of-cubes (car l))))
        (if (= n (sum-of-cubes (car (cdr l))))
            n
            (loop (cdr l)))))

    (loop (weighted-pairs (naturals 1) (naturals 1) sum-of-cubes))))

(define (interpret code)
  (let ((env (make-env builtins)))
    (eval '(define (cons x y) (lambda (m) (m x y))) env)
    (eval '(define (car p) (p (lambda (x y) x))) env)
    (eval '(define (cdr p) (p (lambda (x y) y))) env)
    (for/last ((expr (in-list code)))
      (value expr env))))

(struct closure (params body env))

(struct promise (forced val expr env) #:mutable)

(define (eval expr env)
  (cond ((literal? expr) expr)
        ((symbol? expr) (get-var expr env))
        (else (case (car expr)
                ('quote (eval-quote expr env))
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
                   (subst (closure-params op)
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
      (number? expr)))

(define (eval-quote expr env)
  (let ((x (cadr expr)))
    (if (pair? x)
        (eval (list 'cons
                    (list 'quote (car x))
                    (list 'quote (cdr x)))
              env)
        x)))

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
    (symbol? . ,symbol?)
    (boolean? . ,boolean?)
    (number? . ,number?)

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

    ; input/output
    (display . ,display)
    (newline . ,newline)))
