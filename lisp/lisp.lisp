#lang lisp

(define (list . args) args)

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
  (car (cdr (cddr l))))

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (map f (cdr l)))))

(define (memq x l)
  (cond ((null? l) #f)
        ((eq? x (car l)) l)
        (else (memq x (cdr l)))))

(define (eval expr env)
  (cond ((literal? expr) expr)
        ((symbol? expr) (get-var expr env))
        ((type? 'quote expr) (cadr expr))
        ((type? 'lambda expr) (eval-lambda expr env))
        ((type? 'define expr) (eval-define expr env))
        ((type? 'set expr) (eval-set expr env))
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
                 (subst (closure-vars op)
                        args
                        (closure-env op)))))

(define (literal? expr)
  (or (boolean? expr)
      (number? expr)
      (string? expr)))

(define (type? t x)
  (and (pair? x)
       (eq? t (car x))))

(define (eval-lambda expr env)
  (let ((vars (cadr expr))
        (body (cddr expr)))
    (make-closure vars body env)))

(define (eval-define expr env)
  (let ((var (if (symbol? (cadr expr))
                 (cadr expr)
                 (caadr expr)))
        (x (if (symbol? (cadr expr))
               (caddr expr)
               (let ((vars (cdadr expr))
                     (body (cddr expr)))
                 (cons 'lambda (cons vars body))))))
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
    (if (null? (cdr exprs))
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
              (if (null? rest)
                  consequent
                  (error "else clause must be last"))
              (let ((alternative (expand-cond rest)))
                (list 'if predicate consequent alternative)))))))

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
  (let ((bindings (cadr expr))
        (body (cddr expr)))
    (let ((vars (map car bindings))
          (exprs (map cadr bindings)))
      (cons (cons 'lambda (cons vars body)) exprs))))

(define (make-closure vars body env)
  (list vars body env))

(define closure-vars car)

(define closure-body cadr)

(define closure-env caddr)

(define (subst vars vals env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) env)
      (error "arity mismatch")))

(define make-frame cons)

(define frame-vars car)

(define frame-vals cdr)

(define (define-var var val env)
  (let ((frame (car env)))
    (define (scan vars vals)
      (cond ((null? vars) (set-car! frame (cons var (frame-vars frame)))
                          (set-cdr! frame (cons val (frame-vals frame))))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))

    (scan (frame-vars frame) (frame-vals frame))))

(define (get-var var env)
  (define (loop env)
    (define (scan vars vals)
      (cond ((null? vars) (loop (cdr env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))

    (if (null? env)
        (error "undefined")
        (let ((frame (car env)))
          (scan (frame-vars frame) (frame-vals frame)))))

  (loop env))

(define (set-var var val env)
  (define (loop env)
    (define (scan vars vals)
      (cond ((null? vars) (loop (cdr env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))

    (if (null? env)
        (error "undefined")
        (let ((frame (car env)))
          (scan (frame-vars frame) (frame-vals frame)))))

  (loop env))

(define builtins
  (list (cons '+ +)
        (cons '- -)
        (cons '* *)
        (cons '/ /)
        (cons '< <)
        (cons '> >)
        (cons '= =)
        (cons 'eq? eq?)
        (cons 'not not)
        (cons 'null? null?)
        (cons 'pair? pair?)
        (cons 'cons cons)
        (cons 'car car)
        (cons 'cdr cdr)
        (cons 'list list)
        (cons 'display display)
        (cons 'newline newline)))

(define (builtin? op)
  (memq op (map car builtins)))
