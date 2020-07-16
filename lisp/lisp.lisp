#lang lisp

(define (eval expr env)
  (cond ((literal? expr) expr)
        ((symbol? expr) (get-var expr env))
        ((type? 'quote expr) (cadr expr))
        ((type? 'lambda expr) (eval-lambda expr env))
        ((type? 'define expr) (eval-define expr env))
        ((type? 'set! expr) (eval-set expr env))
        ((type? 'if expr) (eval-if expr env))
        ((type? 'begin expr) (eval-list (cdr expr) env))
        ((type? 'cond expr) (eval (cond->if expr) env))
        ((type? 'and expr) (eval (and->if expr) env))
        ((type? 'or expr) (eval (or->if expr) env))
        ((type? 'let expr) (eval (let->lambda expr) env))
        (else (let ((vals (map (lambda (x) (eval x env)) expr)))
                (let ((op (car vals))
                      (args (cdr vals)))
                  (apply op args))))))

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

(define (eval-lambda expr env)
  (let ((params (cadr expr))
        (body (cddr expr)))
    (make-closure params body env)))

(define (eval-define expr env)
  (let ((var (if (symbol? (cadr expr))
                 (cadr expr)
                 (caadr expr)))
        (x (if (symbol? (cadr expr))
               (caddr expr)
               (let ((params (cdadr expr))
                     (body (cddr expr)))
                 (cons 'lambda (cons params body))))))
    (let ((val (eval x env)))
      (define-var var val env))))

(define (eval-set expr env)
  (let ((var (cadr expr))
        (x (caddr expr)))
    (let ((val (eval x env)))
      (set-var var val env))))

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
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (let ((predicate (car first))
              (consequent (list->expr (cdr first))))
          (if (eq? 'else predicate)
              consequent
              (let ((alternative (expand-cond rest)))
                (list 'if predicate consequent alternative)))))))

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
  (let ((bindings (cadr expr))
        (body (cddr expr)))
    (let ((params (map car bindings))
          (exprs (map cadr bindings)))
      (cons (cons 'lambda (cons params body)) exprs))))

(define (type? t x)
  (and (pair? x)
       (eq? t (car x))))

(define (last? l) (null? (cdr l)))

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (map f (cdr l)))))

(define (cadr l)
  (car (cdr l)))

(define (cddr l)
  (cdr (cdr l)))

(define (caadr l)
  (car (cadr l)))

(define (cdadr l)
  (cdr (cadr l)))

(define (caddr l)
  (car (cddr l)))

(define (cadddr l)
  (cadr (cddr l)))

(define (make-closure params body env)
  (list params body env))

(define closure-params car)

(define closure-body cadr)

(define closure-env caddr)

(define builtins
  (list
   ; types
   (cons 'null? null?)
   (cons 'pair? pair?)
   (cons 'boolean? boolean?)
   (cons 'number? number?)
   (cons 'symbol? symbol?)
   (cons 'builtin? builtin?)

   ; arithmetic
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)

   ; logic
   (cons '< <)
   (cons '> >)
   (cons '= =)
   (cons 'eq? eq?)
   (cons 'not not)

   ; data structures
   (cons 'cons cons)
   (cons 'car car)
   (cons 'cdr cdr)
   (cons 'list list)

   ; input/output
   (cons 'display display)
   (cons 'newline newline)
   (cons 'read read)

   ; environment
   (cons 'make-env make-env)
   (cons 'subst subst)
   (cons 'define-var define-var)
   (cons 'get-var get-var)
   (cons 'set-var set-var)

   ; application
   (cons 'apply* apply*)))

(let ((env (make-env builtins)))
  (define (loop)
    (display (eval (read) env))
    (newline)
    (loop))

  (loop))
