#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/contract
         racket/function
         racket/list)

(struct register (val) #:mutable)

(struct instruction (expr proc) #:mutable)

(define stack%
  (class object%
    (super-new)

    (field (l '()))

    (define/public (push val)
      (set! l (cons val l)))

    (define/public (pop val)
      (let ((val (car l)))
        (set! l (cdr l))
        val))

    (define/public (empty?)
      (null? l))))

(define vm%
  (class stack%
    (super-new)

    (init regs ops)

    (define reg-table
      (make-hash
       (for/list ((reg (append regs '(pc flag))))
         (cons reg (register '())))))

    (define op-table (make-hash ops))

    (define/public (get reg)
      (register-val (hash-ref reg-table reg)))

    (define/public (set reg val)
      (set-register-val! (hash-ref reg-table reg) val))

    (define/public (op name) (hash-ref op-table name))

    (define/public (execute)
      (let ((insts (send this get 'pc)))
        (unless (null? insts)
          (let ((proc (instruction-proc (car insts))))
            (proc)
            (execute)))))

    (define/public (step)
      (let ((insts (send this get 'pc)))
        (send this set 'pc (cdr insts))))

    (define/public (install code)
      (let ((insts (assemble this code)))
        (send this set 'pc insts)))))

(define (valid? code)
  (for/and ((x (in-list code)))
    (or (symbol? x) (expr? x))))

(define (vm? x) (is-a? x vm%))

(define/contract (make-vm code (ops '()))
  (->* (valid?) ((listof (cons/c symbol? procedure?))) vm?)
  (let ((vm (make-object vm%
              (needed-regs code)
              (needed-ops code ops))))
    (send vm install code)
    vm))

(define (needed-regs code)
  (remove-duplicates
   (append (used-in code 'assign)
           (used-in code 'reg))))

(define (needed-ops code ops)
  (let* ((ns (make-base-namespace))
         (provided (map car ops))
         (required (remove-duplicates (used-in code 'op)))
         (builtins (for/list ((name (remove* provided required)))
                     (cons name (eval name ns)))))
    (append ops builtins)))

(define (assemble vm code)
  (define (update insts labels)
    (for ((inst (in-list insts)))
      (let ((expr (instruction-expr inst)))
        (set-instruction-proc! inst (generate vm expr labels))))
    insts)

  (let loop ((code code) (k update))
    (if (null? code)
        (k '() #hash())
        (loop (cdr code)
              (let ((expr (car code)))
                (lambda (insts labels)
                  (if (symbol? expr)
                      (k insts (hash-set labels expr insts))
                      (k (cons (instruction expr (void)) insts) labels))))))))

(define (expr? expr)
  (case (car expr)
    ('assign (assign? expr))
    ('perform (perform? expr))
    ('test (test? expr))
    ('branch (branch? expr))
    ('goto (goto? expr))
    ('save (save? expr))
    ('restore (restore? expr))
    (else #f)))

(define (generate vm expr labels)
  (case (car expr)
    ('assign (generate-assign vm expr labels))
    ('perform (generate-perform vm expr labels))
    ('test (generate-test vm expr labels))
    ('branch (generate-branch vm expr labels))
    ('goto (generate-goto vm expr labels))
    ('save (generate-save vm expr))
    ('restore (generate-restore vm expr))))

; (assign <reg-name> (op <op-name>) <val-expr> ...)
; (assign <reg-name> (const <const-val>))
; (assign <reg-name> (label <label-name>))
; (assign <reg-name> (reg <reg-name>))
(define (assign? expr)
  (let* ((reg (cadr expr))
         (x (cddr expr)))
    (and (symbol? reg)
         (case (caar x)
           ('op (op-expr? x))
           (else (val-expr? (car x)))))))

(define (generate-assign vm expr labels)
  (let* ((reg (cadr expr))
         (x (cddr expr))
         (proc (case (caar x)
                 ('op (generate-op-expr vm x labels))
                 (else (generate-val-expr vm (car x) labels)))))
    (thunk (send vm set reg (proc))
           (send vm step))))

; (perform (op <op-name>) <val-expr> ...)
(define (perform? expr)
  (op-expr? (cdr expr)))

(define (generate-perform vm expr labels)
  (let ((proc (generate-op-expr vm (cdr expr) labels)))
    (thunk (proc)
           (send vm step))))

; (test (op <op-name>) <val-expr> ...)
(define (test? expr)
  (op-expr? (cdr expr)))

(define (generate-test vm expr labels)
  (let ((proc (generate-op-expr vm (cdr expr) labels)))
    (thunk (send vm set 'flag (proc))
           (send vm step))))

; (branch (label <label-name>))
(define (branch? expr)
  (and (eq? 'label (caadr expr))
       (val-expr? (cadr expr))
       (null? (cddr expr))))

(define (generate-branch vm expr labels)
  (let ((insts (hash-ref labels (cadadr expr))))
    (thunk (if (send vm get 'flag)
               (send vm set 'pc insts)
               (send vm step)))))

; (goto (label <label-name>))
; (goto (reg <reg-name>))
(define (goto? expr)
  (let ((x (cadr expr)))
    (and (or (eq? 'label (car x))
             (eq? 'reg (car x)))
         (val-expr? x)
         (null? (cddr expr)))))

(define (generate-goto vm expr labels)
  (let* ((x (cadr expr))
         (proc (generate-val-expr vm x labels)))
    (thunk (send vm set 'pc (proc)))))

; (save <reg-name>)
(define (save? expr)
  (and (symbol? (cadr expr))
       (null? (cddr expr))))

(define (generate-save vm expr)
  (let ((reg (cadr expr)))
    (thunk (send vm push (send vm get reg))
           (send vm step))))

; (restore <reg-name>)
(define (restore? expr)
  (and (symbol? (cadr expr))
       (null? (cddr expr))))

(define (generate-restore vm expr)
  (let ((reg (cadr expr)))
    (thunk (send vm set reg (send vm pop))
           (send vm step))))

; ((op <op-name>) <val-expr> ...)
(define (op-expr? expr)
  (and (eq? 'op (caar expr))
       (symbol? (cadar expr))
       (null? (cddar expr))
       (andmap val-expr? (cdr expr))))

(define (generate-op-expr vm expr labels)
  (let ((op (send vm op (cadar expr)))
        (procs (map (lambda (x)
                      (generate-val-expr vm x labels))
                    (cdr expr))))
    (thunk (apply op (map call procs)))))

; (const <const-val>)
; (label <label-name>)
; (reg <reg-name>)
(define (val-expr? expr)
  (and (case (car expr)
         ('const (let ((val (cadr expr)))
                   (or (boolean? val)
                       (number? val)
                       (string? val))))
         ('label (symbol? (cadr expr)))
         ('reg (symbol? (cadr expr)))
         (else #f))
       (null? (cddr expr))))

(define (generate-val-expr vm expr labels)
  (case (car expr)
    ('const (let ((val (cadr expr)))
              (thunk val)))
    ('label (let ((insts (hash-ref labels (cadr expr))))
              (thunk insts)))
    ('reg (let ((reg (cadr expr)))
            (thunk (send vm get reg))))))

(define (used-in code type)
  (let loop ((l (flatten code)) (acc '()))
    (cond ((null? l) acc)
          ((eq? type (car l)) (loop (cddr l) (cons (cadr l) acc)))
          (else (loop (cdr l) acc)))))

(define (call proc) (proc))
