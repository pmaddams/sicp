#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/contract
         racket/function
         racket/list)

(define (valid? text)
  (for/and ((x (in-list text)))
    (or (symbol? x) (valid-stmt? x))))

(define (vm? x) (is-a? x vm%))

(define/contract
  (make-vm text
           #:regs (regs '())
           #:ops (ops '())
           #:in (in (void))
           #:type (type vm%))
  (->* (valid?)
       (#:regs (listof symbol?)
        #:ops (listof (cons/c symbol? procedure?))
        #:in namespace-anchor?
        #:type class?)
       vm?)
  (let ((vm (make-object type
              (needed-regs text regs)
              (needed-ops text ops in))))
    (send vm install text)
    vm))

(struct register (val) #:mutable)

(struct instruction (stmt proc) #:mutable)

(define stack%
  (class object%
    (super-new)

    (field (l '()))

    (define/public (push val)
      (set! l (cons val l)))

    (define/public (pop)
      (let ((val (car l)))
        (set! l (cdr l))
        val))

    (define/public (empty?)
      (null? l))))

(define vm%
  (class stack%
    (super-new)

    (init regs ops)
    (field (reg-table (make-hash
                       (for/list ((reg (in-list (append regs '(pc flag)))))
                         (cons reg (register (void))))))
           (op-table (make-hash ops)))

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

    (define/public (advance)
      (let ((insts (send this get 'pc)))
        (send this set 'pc (cdr insts))))

    (define/public (install text)
      (let ((insts (assemble this text)))
        (send this set 'pc insts)))))

(define (assemble vm text)
  (define (update insts labels)
    (for ((inst (in-list insts)))
      (let ((stmt (instruction-stmt inst)))
        (set-instruction-proc! inst (generate vm stmt labels))))
    insts)

  (let loop ((text text) (k update))
    (if (null? text)
        (k '() #hash())
        (loop (cdr text)
              (let ((x (car text)))
                (lambda (insts labels)
                  (if (symbol? x)
                      (k insts (hash-set labels x insts))
                      (k (cons (instruction x (void)) insts) labels))))))))

(define (valid-stmt? stmt)
  (case (car stmt)
    ('assign (valid-assign? stmt))
    ('perform (valid-perform? stmt))
    ('test (valid-test? stmt))
    ('branch (valid-branch? stmt))
    ('goto (valid-goto? stmt))
    ('save (valid-save? stmt))
    ('restore (valid-restore? stmt))
    (else #f)))

(define (generate vm stmt labels)
  (case (car stmt)
    ('assign (generate-assign vm stmt labels))
    ('perform (generate-perform vm stmt labels))
    ('test (generate-test vm stmt labels))
    ('branch (generate-branch vm stmt labels))
    ('goto (generate-goto vm stmt labels))
    ('save (generate-save vm stmt))
    ('restore (generate-restore vm stmt))))

; (assign <reg-name> (op <op-name>) <val-expr> ...)
; (assign <reg-name> (const <const-val>))
; (assign <reg-name> (label <label-name>))
; (assign <reg-name> (reg <reg-name>))
(define (valid-assign? stmt)
  (let* ((reg (cadr stmt))
         (x (caddr stmt)))
    (and (symbol? reg)
         (case (car x)
           ('op (valid-op-expr? (cddr stmt)))
           (else (valid-val-expr? x))))))

(define (generate-assign vm stmt labels)
  (let* ((reg (cadr stmt))
         (x (caddr stmt))
         (proc (case (car x)
                 ('op (generate-op-expr vm (cddr stmt) labels))
                 (else (generate-val-expr vm x labels)))))
    (thunk (send vm set reg (proc))
           (send vm advance))))

; (perform (op <op-name>) <val-expr> ...)
(define (valid-perform? stmt)
  (and (eq? 'perform (car stmt))
       (valid-op-expr? (cdr stmt))))

(define (generate-perform vm stmt labels)
  (let ((proc (generate-op-expr vm (cdr stmt) labels)))
    (thunk (proc)
           (send vm advance))))

; (test (op <op-name>) <val-expr> ...)
(define (valid-test? stmt)
  (and (eq? 'test (car stmt))
       (valid-op-expr? (cdr stmt))))

(define (generate-test vm stmt labels)
  (let ((proc (generate-op-expr vm (cdr stmt) labels)))
    (thunk (send vm set 'flag (proc))
           (send vm advance))))

; (branch (label <label-name>))
(define (valid-branch? stmt)
  (and (eq? 'branch (car stmt))
       (let ((x (cadr stmt)))
         (and (eq? 'label (car x))
              (valid-val-expr? x)))
       (null? (cddr stmt))))

(define (generate-branch vm stmt labels)
  (let* ((x (cadr stmt))
         (insts (hash-ref labels (cadr x))))
    (thunk (if (send vm get 'flag)
               (send vm set 'pc insts)
               (send vm advance)))))

; (goto (label <label-name>))
; (goto (reg <reg-name>))
(define (valid-goto? stmt)
  (and (eq? 'goto (car stmt))
       (let ((x (cadr stmt)))
         (and (or (eq? 'label (car x))
                  (eq? 'reg (car x)))
              (valid-val-expr? x)))
       (null? (cddr stmt))))

(define (generate-goto vm stmt labels)
  (let* ((x (cadr stmt))
         (proc (generate-val-expr vm x labels)))
    (thunk (send vm set 'pc (proc)))))

; (save <reg-name>)
(define (valid-save? stmt)
  (and (eq? 'save (car stmt))
       (symbol? (cadr stmt))
       (null? (cddr stmt))))

(define (generate-save vm stmt)
  (let ((reg (cadr stmt)))
    (thunk (send vm push (send vm get reg))
           (send vm advance))))

; (restore <reg-name>)
(define (valid-restore? stmt)
  (and (eq? 'restore (car stmt))
       (symbol? (cadr stmt))
       (null? (cddr stmt))))

(define (generate-restore vm stmt)
  (let ((reg (cadr stmt)))
    (thunk (send vm set reg (send vm pop))
           (send vm advance))))

; ((op <op-name>) <val-expr> ...)
(define (valid-op-expr? expr)
  (and (eq? 'op (caar expr))
       (symbol? (cadar expr))
       (null? (cddar expr))
       (andmap valid-val-expr? (cdr expr))))

(define (generate-op-expr vm expr labels)
  (let ((op (send vm op (cadar expr)))
        (procs (map (lambda (x)
                      (generate-val-expr vm x labels))
                    (cdr expr))))
    (thunk (apply op (map call procs)))))

; (const <const-val>)
; (label <label-name>)
; (reg <reg-name>)
(define (valid-val-expr? expr)
  (and (case (car expr)
         ('const #t)
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

(define (needed-regs text regs)
  (remove-duplicates
   (append regs
           (used-in text 'assign)
           (used-in text 'reg))))

(define (needed-ops text ops in)
  (let* ((ns (if (void? in)
                 (make-base-namespace)
                 (namespace-anchor->namespace in)))
         (provided (map car ops))
         (required (remove-duplicates (used-in text 'op)))
         (builtins (for/list ((name (in-list (remove* provided required))))
                     (cons name (eval name ns)))))
    (append ops builtins)))

(define (used-in text type)
  (flatten
   (for/list ((stmt (in-list text))
              #:when (list? stmt))
     (for/list ((x (in-list stmt))
                #:when (and (list? x)
                            (eq? type (car x))))
       (cadr x)))))

(define (call proc) (proc))
