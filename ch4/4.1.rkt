#lang sicp

(define enclosing-environment cdr)

(define first-frame car)

(define the-empty-environment '())

(define make-frame cons)

(define frame-variables car)

(define frame-values cdr)

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars)
         (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars)
             (length vals))
          (error "too many arguments:" vars vals)
          (error "too few arguments:" vars vals))))

(define (lookup-variable-value var env)
  (letrec ((scan (lambda (vars vals)
                   (cond ((null? vars) (env-loop (enclosing-environment env)))
                         ((eq? var (car vars)) (car vals))
                         (else (scan (cdr vars) (cdr vals))))))
           (env-loop (lambda (env)
                       (if (eq? env the-empty-environment)
                           (error "unbound variable:" var)
                           (let ((frame (first-frame env)))
                             (scan (frame-variables frame)
                                   (frame-values frame)))))))
    (env-loop env)))

(define (set-variable-value! var val env)
  (letrec ((scan (lambda (vars vals)
                   (cond ((null? vars) (env-loop (enclosing-environment env)))
                         ((eq? var (car vars)) (set-car! vals val))
                         (else (scan (cdr vars) (cdr vals))))))
           (env-loop (lambda (env)
                       (if (eq? env the-empty-environment)
                           (error "set!: unbound variable:" var)
                           (let ((frame (first-frame env)))
                             (scan (frame-variables frame)
                                   (frame-values frame)))))))
    (env-loop env)))

(define (define-variable! var val env)
  (letrec ((frame (first-frame env))
           (scan (lambda (vars vals)
                   (cond ((null? vars) (add-binding-to-frame! var val frame))
                         ((eq? var (car vars)) (set-car! vals val))
                         (else (scan (cdr vars) (cdr vals)))))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define primitive-implementation cadr)

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment)))
    (define-variable! '#t #t initial-env)
    (define-variable! '#f #f initial-env)
    initial-env))

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)))

(define variable? symbol?)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define text-of-quotation cadr)

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define assignment-variable cadr)

(define assignment-value caddr)

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-variable exp) env)
                       env)
  'ok)

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-variable exp) env)
    env)
  'ok)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (if? exp)
  (tagged-list? exp 'if))

(define if-predicate cadr)

(define if-consequent caddr)

(define (if-alternative exp)
  (if (not (null? cdddr exp))
      (cadddr exp)
      #f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative) env)))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define lambda-parameters cadr)

(define lambda-body cddr)

(define (begin? exp)
  (tagged-list? exp 'begin))

(define begin-actions cdr)

(define (last-exp? seq)
  (null? (cdr seq)))

(define first-exp car)

(define rest-exps cdr)

(define (make-begin seq)
  (cons 'begin seq))

(define (sequence->exp seq)
  (cond ((null? seq) '())
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define application? pair?)

(define operator car)

(define operands cdr)

(define no-operands? null?)

(define first-operand car)

(define rest-operands cdr)

(define (cond? exp)
  (tagged-list? exp 'cond))

(define cond-clauses cdr)

(define cond-predicate car)

(define cond-actions cdr)

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      '#f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "cond->if: else clause must be last:" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define procedure-parameters cadr)

(define procedure-body caddr)

(define procedure-environment cadddr)

(define (eval-sequence exps env)
  (eval (first-exp exps) env)
  (if (not (last-exp? exps))
      (eval-sequence (rest-exps exps) env)))

(define (eval exp env)
  (cond ((self-evaluating? exp)
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((quoted? exp)
         (text-of-quotation exp))
        ((assignment? exp)
         (eval-assignment exp env))
        ((definition? exp)
         (eval-definition exp env))
        ((if? exp)
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence exp env))
        ((cond? exp)
         (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "eval: unknown expression type:" exp))))

(define (apply proc args)
  (letrec ((apply-primitive-procedure
            (lambda (proc args)
              (apply (primitive-implementation proc)
                     args))))
    (cond ((primitive-procedure? proc)
           (apply-primitive-procedure proc args))
          ((compound-procedure? proc)
           (eval-sequence (procedure-body proc)
                          (extend-environment (procedure-parameters proc)
                                              args
                                              (procedure-environment proc))))
          (else
           (error "apply: unknown procedure type:" proc)))))

(define input-prompt ">>> ")

(define (prompt-for-input string)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (user-print output)))
  (driver-loop))

(driver-loop)
