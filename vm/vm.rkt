#lang racket/base

(provide vm%)

(require racket/class
         racket/dict)

(define register%
  (class object%
    (super-new)

    (field (var (void)))

    (define/public (get) var)

    (define/public (set val)
      (set! var val))))

(define stack%
  (class object%
    (super-new)

    (field (l '()))

    (define/public (empty?)
      (null? l))

    (define/public (push val)
      (set! l (cons val l)))

    (define/public (pop)
      (let ((val (car l)))
        (set! l (cdr l))
        val))))

(define vm%
  (class stack%
    (super-new)

    (init regs ops)

    (define regtab
      (let ((tab (make-hash)))
        (for ((reg (append '(pc flag) regs)))
          (if (hash-has-key? tab reg)
              (error "duplicate register:" reg)
              (hash-set! tab reg (new register%))))
        tab))

    (define optab
      (let ((names (map car ops))
            (procs (map cdr ops)))
        (make-hash (map cons names procs))))

    (define/public (get reg)
      (send (dict-ref regtab reg) get))

    (define/public (set reg val)
      (send (dict-ref regtab reg) set val))

    (define/public (op name)
      (dict-ref optab name))

    (define/public (install code)
      (send this set 'pc
            (assemble this code)))

    (define/public (advance)
      (send this set 'pc
            (cdr (send this get 'pc))))

    (define/public (execute)
      (let ((insts (send this get 'pc)))
        (unless (null? insts)
          ((instruction-proc (car insts)))
          (execute))))))

(struct instruction (expr (proc #:mutable)))

(define (assemble vm code)
  (define (update insts labels)
    (for ((inst insts))
      (set-instruction-proc! inst (generate (instruction-expr inst) vm)))
    insts)

  (define (loop code k)
    (if (null? code)
        (update '() #hash())
        (loop (cdr code)
              (lambda (insts labels)
                (let ((next (car code)))
                  (if (symbol? next)
                      (k insts (dict-set labels next insts))
                      (k (cons (instruction next (void)) insts) labels)))))))

  (loop code update))

(define (generate vm expr labels)
  (case (car expr)
    ('assign (generate-assign vm expr labels))
    ('test (generate-test vm expr labels))
    ('branch (generate-branch vm expr labels))
    ('goto (generate-goto vm expr labels))
    ('save (generate-save vm expr))
    ('restore (generate-restore vm expr))
    ('perform (generate-perform vm expr labels))
    (else (error "invalid expression:" expr))))

(define (generate-assign vm expr labels)
  (let* ((reg (assign-reg expr))
         (val-expr (assign-val-expr expr))
         (proc (if (op-expr? val-expr)
                   (generate-op-expr vm labels val-expr)
                   (generate-primitive-expr vm labels (car val-expr)))))
    (lambda ()
      (send reg write proc)
      (send vm advance))))

(define (assign-reg expr)
  (cadr expr))

(define (assign-val-expr expr)
  (cddr expr))

(define (generate-test vm expr labels)
  (let* ((cond-expr (test-cond-expr expr))
         (proc (if (op-expr? cond-expr)
                   (generate-op-expr vm labels cond-expr)
                   (error "invalid expression:" expr))))
    (lambda ()
      (send vm set 'flag proc)
      (send vm advance))))

(define (test-cond-expr expr)
  (cdr expr))

(define (generate-branch vm expr labels)
  (let* ((loc-expr (branch-loc-expr expr))
         (insts (if (label-expr? loc-expr)
                    (dict-ref labels (label-expr-label loc-expr))
                    (error "invalid expression:" expr))))
    (lambda ()
      (if (send vm get 'flag)
          (send vm set 'pc insts)
          (send vm advance)))))

(define (branch-loc-expr expr)
  (cadr expr))

(define (label-expr? expr)
  (tagged-list? expr 'label))

(define label-expr-label cadr)

(define (generate-goto vm expr labels)
  (let ((loc-expr (goto-loc-expr expr)))
    (cond ((label-expr? loc-expr)
           (let ((insts (dict-ref labels (label-expr-label loc-expr))))
             (lambda ()
               (send vm set 'pc insts))))
          ((reg-expr? loc-expr)
           (let ((reg (reg-expr-reg loc-expr)))
             (lambda ()
               (send vm set 'pc (send vm get reg)))))
          (else (error "invalid expression:" expr)))))

(define (goto-loc-expr expr)
  (cadr expr))

(define (generate-save vm expr)
  (let ((reg (stack-expr-reg expr)))
    (lambda ()
      (send vm push (send vm get reg))
      (send vm advance))))

(define (generate-restore vm expr)
  (let ((reg (stack-expr-reg expr)))
    (lambda ()
      (send vm set reg (send vm pop))
      (send vm advance))))

(define (stack-expr-reg expr)
  (cadr expr))

(define (generate-perform vm expr labels)
  (let ((op-expr (perform-op-expr expr)))
    (if (op-expr? op-expr)
        (let ((proc (generate-op-expr vm labels op-expr)))
          (lambda ()
            (proc)
            (send vm advance)))
        (error "invalid expression:" expr))))

(define (perform-op-expr expr)
  (cdr expr))

(define (op-expr? expr)
  (and (pair? expr)
       (tagged-list? (car expr) 'op)))

(define (generate-op-expr vm labels expr)
  (let ((primitive-proc (send vm op (op-expr-op expr)))
        (primitive-operands (map (lambda (expr)
                      (generate-primitive-expr vm labels expr))
                    (op-expr-operands expr))))
    (lambda ()
      (apply primitive-proc (map (lambda (proc) (proc)) primitive-operands)))))

(define op-expr-op cadar)

(define op-expr-operands cdr)

(define (generate-primitive-expr vm labels expr)
  (cond ((const-expr? expr)
         (let ((val (cadr expr)))
           (lambda () val)))
        ((reg-expr? expr)
         (let ((reg (cadr expr)))
           (lambda () (send vm get reg))))
        ((label-expr? expr)
         (let ((insts (dict-ref labels (cadr expr))))
           (lambda () insts)))
        (else (error "invalid expression:" expr))))

(define (const-expr? expr)
  (tagged-list? expr 'const))

(define const-expr-val cadr)

(define (reg-expr? expr)
  (tagged-list? expr 'reg))

(define reg-expr-reg cadr)

(define (tagged-list? expr tag)
  (and (pair? expr)
       (eq? (car expr) tag)))
