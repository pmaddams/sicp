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
  (sequence-text (compile-list code 'val 'next)))

(struct sequence (needs modifies text))

(struct subroutine (label env))

(define (compile expr target linkage)
  (cond ((literal? expr) (compile-literal expr target linkage))
        ((variable? expr) (compile-variable expr target linkage))
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
            (sequence '() (list target)
                      `((assign ,target (const ,expr))))))

(define variable? symbol?)

(define (compile-variable expr target linkage)
  (end-with linkage
            (sequence '(env) (list target)
                      `((assign ,target (op get-var) (const ,expr) (reg env))))))

(define (compile-quote expr target linkage)
  (end-with linkage
            (sequence '() (list target)
                      `((assign ,target (const ,(cadr expr)))))))

(define (compile-lambda expr target linkage)
  (let ((label (gensym 'lambda-))
        (after-label (gensym 'after-lambda-)))
    (let ((linkage* (if (eq? linkage 'next) after-label linkage)))
      (generate-sequence
       (generate-unconnected
        (end-with linkage*
                  (sequence '(env) (list target)
                            `((assign ,target (op subroutine) (label ,label) (reg env)))))
        (compile-lambda-body expr label))
       (generate-label after-label)))))

(define (compile-lambda-body expr label)
  (let ((params (cadr expr))
        (body (cddr expr)))
    (generate-sequence
     (sequence '(env proc args) '(env)
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
         (seq (compile x 'val 'next)))
    (end-with linkage
              (generate-preserving
               '(env)
               seq
               (sequence
                 '(env val) (list target)
                 `((assign ,target (op define-var) (const ,var) (reg val) (reg env))))))))

(define (compile-set expr target linkage)
  (let ((var (cadr expr))
        (seq (compile (caddr expr) 'val 'next)))
    (end-with linkage
              (generate-preserving
               '(env)
               seq
               (sequence
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
      (let ((predicate-seq (compile predicate 'val 'next))
            (consequent-seq (compile consequent target consequent-linkage))
            (alternative-seq (compile alternative target linkage)))
        (generate-preserving
         '(env continue)
         predicate-seq
         (generate-sequence
          (sequence '(val) '()
                    `((test (op false?) (reg val))
                      (branch (label ,else-label))))
          (generate-alternatives
           (generate-sequence
            (generate-label then-label)
            consequent-seq)
           (generate-sequence
            (generate-label else-label)
            alternative-seq))
          (generate-label after-label)))))))

(define (compile-list exprs target linkage)
  (if (last? exprs)
      (compile (car exprs) target linkage)
      (generate-preserving
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
  (let ((proc-seq (compile (car expr) 'proc 'next))
        (compiled-vals (reverse (map (lambda (x) (compile x 'val 'next))
                                     (cdr expr)))))
    (generate-preserving
     '(env continue)
     proc-seq
     (generate-preserving
      '(proc continue)
      (construct-args compiled-vals)
      (compile-call target linkage)))))

(define (construct-args compiled-vals)
  (if (null? compiled-vals)
      (sequence '() '(args)
                '((assign args (const ()))))
      (let ((last-arg-seq
             (generate-sequence
              (car compiled-vals)
              (sequence '(val) '(args)
                        '((assign args (op list) (reg val)))))))
        (if (last? compiled-vals)
            last-arg-seq
            (generate-preserving
             '(env)
             last-arg-seq
             (construct-init-args
              (cdr compiled-vals)))))))

(define (construct-init-args compiled-vals)
  (let ((next-arg-seq
         (generate-preserving
          '(args)
          (car compiled-vals)
          (sequence '(val args) '(args)
                    '((assign args (op cons) (reg val) (reg args)))))))
    (if (last? compiled-vals)
        next-arg-seq
        (generate-preserving
         '(env)
         next-arg-seq
         (construct-init-args (cdr compiled-vals))))))

(define (compile-call target linkage)
  (let ((builtin-label (gensym 'builtin-))
        (subroutine-label (gensym 'subroutine-))
        (after-label (gensym 'after-call-)))
    (let ((subroutine-linkage (if (eq? linkage 'next) after-label linkage)))
      (generate-sequence
       (sequence '(proc) '()
                 `((test (op procedure?) (reg proc))
                   (branch (label ,builtin-label))))
       (generate-alternatives
        (generate-sequence
         (generate-label subroutine-label)
         (compile-subroutine-call target subroutine-linkage))
        (generate-sequence
         (generate-label builtin-label)
         (end-with linkage
                   (sequence '(proc args) (list target)
                             `((assign ,target (op apply) (reg proc) (reg args)))))))
       (generate-label after-label)))))

(define (compile-subroutine-call target linkage)
  (case target
    ('val (case linkage
            ('return
             (sequence '(proc continue) all-regs
                       '((assign val (op subroutine-label) (reg proc))
                         (goto (reg val)))))
            (else
             (sequence '(proc) all-regs
                       `((assign continue (label ,linkage))
                         (assign val (op subroutine-label) (reg proc))
                         (goto (reg val)))))))
    (else (case linkage
            ('return (error "compilation error"))
            (else (let ((return-label (gensym 'return-)))
                    (sequence '(proc) all-regs
                              `((assign continue (label ,return-label))
                                (assign val (op subroutine-label) (reg proc))
                                (goto (reg val))
                                ,return-label
                                (assign ,target (reg val))
                                (goto (label ,linkage))))))))))

(define (end-with linkage seq)
  (generate-preserving
   '(continue)
   seq
   (case linkage
     ('next (generate-no-op))
     ('return (sequence '(continue) '()
                        '((goto (reg continue)))))
     (else (sequence '() '()
                     `((goto (label ,linkage))))))))

(define (generate-preserving regs seq1 seq2)
  (if (null? regs)
      (generate-sequence seq1 seq2)
      (let ((reg (car regs)))
        (if (and (needs? reg seq2)
                 (modifies? reg seq1))
            (generate-preserving
             (cdr regs)
             (sequence
               (set-union (list reg)
                          (sequence-needs seq1))
               (set-subtract (sequence-modifies seq1)
                             (list reg))
               (append `((save ,reg))
                       (sequence-text seq1)
                       `((restore ,reg))))
             seq2)
            (generate-preserving (cdr regs) seq1 seq2)))))

(define (generate-alternatives seq1 seq2)
  (sequence
    (set-union (sequence-needs seq1)
               (sequence-needs seq2))
    (set-union (sequence-modifies seq1)
               (sequence-modifies seq2))
    (append (sequence-text seq1)
            (sequence-text seq2))))

(define (generate-unconnected seq body-seq)
  (sequence
    (sequence-needs seq)
    (sequence-modifies seq)
    (append (sequence-text seq)
            (sequence-text body-seq))))

(define (generate-sequence . args)
  (define (combine seq1 seq2)
    (sequence (set-union (sequence-needs seq1)
                         (set-subtract (sequence-needs seq2)
                                       (sequence-modifies seq1)))
              (set-union (sequence-modifies seq1)
                         (sequence-modifies seq2))
              (append (sequence-text seq1)
                      (sequence-text seq2))))

  (let loop ((l args))
    (if (null? l)
        (generate-no-op)
        (combine (car l) (loop (cdr l))))))

(define (generate-label label)
  (sequence '() '() (list label)))

(define (generate-no-op)
  (sequence '() '() '()))

(define (needs? reg seq)
  (memq reg (sequence-needs seq)))

(define (modifies? reg seq)
  (memq reg (sequence-modifies seq)))

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
