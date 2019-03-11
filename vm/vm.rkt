#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/function)

(struct register (val) #:mutable)

(struct instruction (text proc) #:mutable)

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

(define (assemble vm code)
  (define (augment insts labels)
    (for ((inst (in-list insts)))
      (let ((expr (instruction-text inst)))
        (set-instruction-proc! inst (generate vm expr labels))))
    insts)

  (let loop ((code code) (k augment))
    (if (null? code)
        (k '() #hash())
        (loop (cdr code)
              (let ((text (car code)))
                (lambda (insts labels)
                  (if (symbol? text)
                      (k insts (hash-set labels text insts))
                      (k (cons (instruction text (void)) insts) labels))))))))

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

(define (generate-assign vm expr labels)
  (let* ((reg (cadr expr))
         (expr* (cddr expr))
         (exec (if (eq? 'op (caar expr*))
                   (generate-op-expr vm expr* labels)
                   (generate-val-expr vm (car expr*) labels))))
    (thunk (send vm set reg (exec))
           (send vm step))))

; (perform (op <op-name>) <val-expr> ...)

(define (generate-perform vm expr labels)
  (let ((exec (generate-op-expr vm (cdr expr) labels)))
    (thunk (exec)
           (send vm step))))

; (test (op <op-name>) <val-expr> ...)

(define (generate-test vm expr labels)
  (let ((exec (generate-op-expr vm expr labels)))
    (thunk (send vm set 'flag (exec))
           (send vm step))))

; (branch (label <label-name>))

(define (generate-branch vm expr labels)
  (let ((insts (hash-ref labels (cadadr expr))))
    (thunk (if (send vm get 'flag)
               (send vm set 'pc insts)
               (send vm step)))))

; (goto (label <label-name>))
; (goto (reg <reg-name>))

(define (generate-goto vm expr labels)
  (let ((expr* (cadr expr)))
    (case (car expr*)
      ('label (thunk (hash-ref labels (cadr expr*))))
      ('reg (thunk (send vm get (cadr expr*)))))))

; (save <reg-name>)

(define (generate-save vm expr)
  (let ((reg (cadadr expr)))
    (thunk (send vm push (send vm get reg))
           (send vm step))))

; (restore <reg-name>)

(define (generate-restore vm expr)
  (let ((reg (cadadr expr)))
    (thunk (send vm set reg (send vm pop))
           (send vm step))))

; ((op <op-name>) <val-expr> ...)

(define (generate-op-expr vm expr labels)
  (let ((proc (send vm op (cadar expr)))
        (execs (map (lambda (x)
                      (generate-val-expr vm x labels))
                    (cdr expr))))
    (thunk (apply proc (map run execs)))))

; (const <const-val>)
; (label <label-name>)
; (reg <reg-name>)

(define (generate-val-expr vm expr labels)
  (case (car expr)
    ('const (thunk (cadr expr)))
    ('label (thunk (hash-ref labels (cadr expr))))
    ('reg (thunk (send vm get (cadr expr))))))

(define (run exec) (exec))
