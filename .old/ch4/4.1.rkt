#lang sicp

(#%require (only racket/base
                 make-hash
                 hash-ref
                 hash-set!))

(define (make-table)
  (let* ((table (make-hash))
         (get (lambda (key)
                (hash-ref table key #f)))
         (put (lambda (key value)
                (hash-set! table key value)))
         (dispatch (lambda (m)
                     (case m
                       ('get get)
                       ('put put)
                       (else (error "table: unknown method:" m))))))
    dispatch))

(define (get table key)
  ((table 'get) key))

(define (put table key value)
  ((table 'put) key value))

(define (make-frame vars vals)
  (let ((frame (make-table)))
    (letrec ((m (lambda (vars vals)
                  (if (null? vars)
                      frame
                      (begin (put frame
                                  (car vars)
                                  (car vals))
                             (m (cdr vars)
                                (cdr vals)))))))
      (cond ((= (length vars)
                (length vals))
             (m vars vals))
            ((< (length vars)
                (length vals))
             (error "too many arguments:" vars vals))
            (else
             (error "too few arguments:" vars vals))))))

(define (extend-environment vars vals env)
  (cons (make-frame vars vals) env))

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)))

(define variable? symbol?)

(define (define-variable! var val env)
  (let ((frame (car env)))
    (put frame var val)))

(define (lookup-variable-value var env)
  (letrec ((l (lambda (env)
                (if (null? env)
                    (error "unbound variable:" var)
                    (let* ((frame (car env))
                           (val (get frame var)))
                      (if val
                          val
                          (l (cdr env))))))))
    (l env)))

(define (set-variable-value! var val env)
  (letrec ((s (lambda (env)
                (if (null? env)
                    (error "set!: unbound variable:" var)
                    (let ((frame (car env)))
                      (if (get frame var)
                          (put frame var val)
                          (s (cdr env))))))))
    (s env)))

(define (tagged-list? tag)
  (lambda (exp)
    (and (pair? exp)
         (eq? (car exp) tag))))

(define quoted?
  (tagged-list? 'quote))

(define text-of-quotation cadr)

(define assignment?
  (tagged-list? 'set!))

(define assignment-variable cadr)

(define assignment-value caddr)

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-variable exp) env)
                       env))

(define (make-lambda parameters body)
  (cons 'lambda
        (cons parameters body)))

(define lambda?
  (tagged-list? 'lambda))

(define lambda-parameters cadr)

(define lambda-body cddr)

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define compound-procedure?
  (tagged-list? 'procedure))

(define procedure-parameters cadr)

(define procedure-body caddr)

(define procedure-environment cadddr)

(define definition?
  (tagged-list? 'define))

(define (definition-variable exp)
  (if (variable? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (variable? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-variable exp) env)
    env))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define if?
  (tagged-list? 'if))

(define if-predicate cadr)

(define if-consequent caddr)

(define (if-alternative exp)
  (and (not (null? (cdddr exp)))
       (cadddr exp)))

(define (eval-if exp env)
  (if (eval (if-predicate exp) env)
      (eval (if-consequent exp) env)
      (eval (if-alternative) env)))

(define (make-begin seq)
  (cons 'begin seq))

(define begin?
  (tagged-list? 'begin))

(define begin-actions cdr)

(define first-exp car)

(define (last-exp? seq)
  (null? (cdr seq)))

(define rest-exps cdr)

(define (eval-sequence exps env)
  (letrec ((e (lambda (exps)
                (eval (first-exp exps) env)
                (if (not (last-exp? exps))
                    (e (rest-exps exps))))))
    (e exps)))

(define cond?
  (tagged-list? 'cond))

(define cond-clauses cdr)

(define cond-predicate car)

(define cond-actions cdr)

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (sequence->exp seq)
  (cond ((null? seq) '())
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (expand-clauses clauses)
  (and (not (null? clauses))
       (let ((first (car clauses))
             (rest (cdr clauses)))
         (if (cond-else-clause? first)
             (if (null? rest)
                 (sequence->exp (cond-actions first))
                 (error "cond: else clause must be last:" clauses))
             (make-if (cond-predicate first)
                      (sequence->exp (cond-actions first))
                      (expand-clauses rest))))))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define application? pair?)

(define operator car)

(define operands cdr)

(define first-operand car)

(define rest-operands cdr)

(define no-operands? null?)

(define (list-of-values exps env)
  (letrec ((l (lambda (exps)
                (if (no-operands? exps)
                    '()
                    (cons (eval (first-operand exps) env)
                          (l (rest-operands exps)))))))
    (l exps)))

(define primitive-procedures
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define primitive-procedure?
  (tagged-list? 'primitive))

(define primitive-implementation cadr)

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (apply-metacircular proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         (eval-sequence (procedure-body proc)
                        (extend-environment (procedure-parameters proc)
                                            args
                                            (procedure-environment proc))))
        (else (error "apply: unknown procedure type:" proc))))

(define (eval exp env)
  (cond ((self-evaluating? exp)
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((quoted? exp)
         (text-of-quotation exp))
        ((assignment? exp)
         (eval-assignment exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((definition? exp)
         (eval-definition exp env))
        ((if? exp)
         (eval-if exp env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp)
         (eval (cond->if exp) env))
        ((application? exp)
         (apply-metacircular (eval (operator exp) env)
                             (list-of-values (operands exp) env)))
        (else (error "eval: unknown expression type:" exp))))

(define (setup-environment)
  (let ((env (extend-environment (primitive-procedure-names)
                                 (primitive-procedure-objects)
                                 '())))
    (define-variable! '#t #t env)
    (define-variable! '#f #f env)
    env))

(define (displayln x)
  (display x)
  (newline))

(define (print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (displayln object)))

(define (scheme)
  (let ((env (setup-environment)))
    (letrec ((loop (lambda ()
                     (displayln "> ")
                     (let* ((input (read))
                            (output (eval input env)))
                       (print output))
                     (loop))))
      (loop))))

(scheme)