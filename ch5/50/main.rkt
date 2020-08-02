#lang racket/base

; Exercise 5.50

(provide (all-defined-out))

(require racket/class
         racket/set
         lisp/env
         vm)

(define-namespace-anchor here)

(define (interpret code)
  (and (not (null? code))
       (let ((vm (make-vm (compile* code) #:regs all-regs #:in here)))
         (send vm set 'env (make-env builtins))
         (send vm execute)
         (send vm get 'val))))

(define (compile* code)
  (output-text (compile-list code 'val 'next)))

(struct output (needs modifies text))

(struct subroutine (label env))

(define (compile expr target linkage)
  (cond ((literal? expr) (compile-literal expr target linkage))
        ((symbol? expr) (compile-variable expr target linkage))
        (else (case (car expr)
                ('quote (compile-quote expr target linkage))
                ('lambda (compile-lambda expr target linkage))
                ('define (compile-define expr target linkage))
                ('set! (compile-set expr target linkage))
                ('if (compile-if expr target linkage))
                ('begin (compile-list (cdr expr) target linkage))
                ('cond (compile (cond->if expr) target linkage))
                ('and (compile (and->if expr) target linkage))
                ('or (compile (or->if expr) target linkage))
                ('let (compile (let->lambda expr) target linkage))
                (else (compile-application expr target linkage))))))

(define (literal? expr)
  (or (boolean? expr)
      (number? expr)))

(define (compile-literal expr target linkage)
  (end-with linkage
            (output '() (list target)
                    `((assign ,target (const ,expr))))))

(define (compile-variable expr target linkage)
  (end-with linkage
            (output '(env) (list target)
                    `((assign ,target (op get-var) (const ,expr) (reg env))))))

(define (compile-quote expr target linkage)
  (end-with linkage
            (output '() (list target)
                    `((assign ,target (const ,(cadr expr)))))))

(define (compile-lambda expr target linkage)
  (let ((label (gensym 'lambda-))
        (after-label (gensym 'after-lambda-)))
    (let ((linkage* (if (eq? linkage 'next) after-label linkage)))
      (append-output
       (embed-output
        (end-with linkage*
                  (output '(env) (list target)
                          `((assign ,target (op subroutine) (label ,label) (reg env)))))
        (compile-lambda-body expr label))
       (label-output after-label)))))

(define (compile-lambda-body expr label)
  (let ((params (cadr expr))
        (body (cddr expr)))
    (append-output
     (output '(env proc args) '(env)
             `(,label
               (assign env (op subroutine-env) (reg proc))
               (assign env (op subst) (const ,params) (reg args) (reg env))))
     (compile-list body 'val 'return))))

(define (compile-define expr target linkage)
  (let* ((var (if (symbol? (cadr expr))
                  (cadr expr)
                  (caadr expr)))
         (x (if (symbol? (cadr expr))
                (caddr expr)
                (let ((params (cdadr expr))
                      (body (cddr expr)))
                  (cons 'lambda (cons params body)))))
         (out (compile x 'val 'next)))
    (end-with linkage
              (output-preserving
               '(env)
               out
               (output
                '(env val) (list target)
                `((assign ,target (op define-var) (const ,var) (reg val) (reg env))))))))

(define (compile-set expr target linkage)
  (let ((var (cadr expr))
        (out (compile (caddr expr) 'val 'next)))
    (end-with linkage
              (output-preserving
               '(env)
               out
               (output
                '(env val) (list target)
                `((assign ,target (op set-var) (const ,var) (reg val) (reg env))))))))

(define (compile-if expr target linkage)
  (let ((predicate (cadr expr))
        (consequent (caddr expr))
        (alternative (cadddr expr))
        (then-label (gensym 'then-))
        (else-label (gensym 'else-))
        (after-label (gensym 'after-if-)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-label linkage)))
      (let ((predicate-out (compile predicate 'val 'next))
            (consequent-out (compile consequent target consequent-linkage))
            (alternative-out (compile alternative target linkage)))
        (output-preserving
         '(env continue)
         predicate-out
         (append-output
          (output '(val) '()
                  `((test (op false?) (reg val))
                    (branch (label ,else-label))))
          (parallel-output
           (append-output
            (label-output then-label)
            consequent-out)
           (append-output
            (label-output else-label)
            alternative-out))
          (label-output after-label)))))))

(define (compile-list exprs target linkage)
  (if (last? exprs)
      (compile (car exprs) target linkage)
      (output-preserving
       '(env continue)
       (compile (car exprs) target 'next)
       (compile-list (cdr exprs) target linkage))))

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

(define (compile-application expr target linkage)
  (let ((proc-out (compile (car expr) 'proc 'next))
        (compiled-vals (reverse (map (lambda (x) (compile x 'val 'next))
                                     (cdr expr)))))
    (output-preserving
     '(env continue)
     proc-out
     (output-preserving
      '(proc continue)
      (construct-args compiled-vals)
      (compile-call target linkage)))))

(define (construct-args compiled-vals)
  (if (null? compiled-vals)
      (output '() '(args)
              '((assign args (const ()))))
      (let ((last-arg-out
             (append-output
              (car compiled-vals)
              (output '(val) '(args)
                      '((assign args (op list) (reg val)))))))
        (if (last? compiled-vals)
            last-arg-out
            (output-preserving
             '(env)
             last-arg-out
             (construct-init-args
              (cdr compiled-vals)))))))

(define (construct-init-args compiled-vals)
  (let ((next-arg-out
         (output-preserving
          '(args)
          (car compiled-vals)
          (output '(val args) '(args)
                  '((assign args (op cons) (reg val) (reg args)))))))
    (if (last? compiled-vals)
        next-arg-out
        (output-preserving
         '(env)
         next-arg-out
         (construct-init-args (cdr compiled-vals))))))

(define (compile-call target linkage)
  (let ((builtin-label (gensym 'builtin-))
        (subroutine-label (gensym 'subroutine-))
        (after-label (gensym 'after-call-)))
    (let ((subroutine-linkage (if (eq? linkage 'next) after-label linkage)))
      (append-output
       (output '(proc) '()
               `((test (op procedure?) (reg proc))
                 (branch (label ,builtin-label))))
       (parallel-output
        (append-output
         (label-output subroutine-label)
         (compile-subroutine-call target subroutine-linkage))
        (append-output
         (label-output builtin-label)
         (end-with linkage
                   (output '(proc args) (list target)
                           `((assign ,target (op apply) (reg proc) (reg args)))))))
       (label-output after-label)))))

(define (compile-subroutine-call target linkage)
  (case target
    ('val (case linkage
            ('return
             (output '(proc continue) all-regs
                     '((assign val (op subroutine-label) (reg proc))
                       (goto (reg val)))))
            (else
             (output '(proc) all-regs
                     `((assign continue (label ,linkage))
                       (assign val (op subroutine-label) (reg proc))
                       (goto (reg val)))))))
    (else (case linkage
            ('return (error "compilation error"))
            (else (let ((return-label (gensym 'return-)))
                    (output '(proc) all-regs
                            `((assign continue (label ,return-label))
                              (assign val (op subroutine-label) (reg proc))
                              (goto (reg val))
                              ,return-label
                              (assign ,target (reg val))
                              (goto (label ,linkage))))))))))

(define (end-with linkage out)
  (output-preserving
   '(continue)
   out
   (case linkage
     ('next (empty-output))
     ('return (output '(continue) '()
                      '((goto (reg continue)))))
     (else (output '() '()
                   `((goto (label ,linkage))))))))

(define (output-preserving regs out1 out2)
  (if (null? regs)
      (append-output out1 out2)
      (let ((reg (car regs)))
        (if (and (needs? reg out2)
                 (modifies? reg out1))
            (output-preserving
             (cdr regs)
             (output
              (set-union (list reg)
                         (output-needs out1))
              (set-subtract (output-modifies out1)
                            (list reg))
              (append `((save ,reg))
                      (output-text out1)
                      `((restore ,reg))))
             out2)
            (output-preserving (cdr regs) out1 out2)))))

(define (parallel-output out1 out2)
  (output
   (set-union (output-needs out1)
              (output-needs out2))
   (set-union (output-modifies out1)
              (output-modifies out2))
   (append (output-text out1)
           (output-text out2))))

(define (embed-output out body-out)
  (output
   (output-needs out)
   (output-modifies out)
   (append (output-text out)
           (output-text body-out))))

(define (append-output . args)
  (define (combine out1 out2)
    (output (set-union (output-needs out1)
                       (set-subtract (output-needs out2)
                                     (output-modifies out1)))
            (set-union (output-modifies out1)
                       (output-modifies out2))
            (append (output-text out1)
                    (output-text out2))))

  (let loop ((l args))
    (if (null? l)
        (empty-output)
        (combine (car l) (loop (cdr l))))))

(define (label-output label)
  (output '() '() (list label)))

(define (empty-output)
  (output '() '() '()))

(define (needs? reg out)
  (memq reg (output-needs out)))

(define (modifies? reg out)
  (memq reg (output-modifies out)))

(define all-regs '(env proc val args continue))

(define (last? l) (null? (cdr l)))

(define (true? x) (not (false? x)))

(define (false? x) (eq? x #f))

(define builtins
  `(; types
    (null? . ,null?)
    (pair? . ,pair?)
    (symbol? . ,symbol?)
    (boolean? . ,boolean?)
    (number? . ,number?)
    (builtin? . ,procedure?)

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
    (apply* . ,apply)))
