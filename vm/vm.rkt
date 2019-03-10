#lang racket/base

(provide (all-defined-out))

(require racket/class)

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

(define (generate-assign vm expr labels)
  (let* ((reg (cadr expr))
         (expr* (cddr expr))
         (exec (if (op-expr? expr*)
                   (generate-op-expr vm expr* labels)
                   (generate-val-expr vm (car expr*) labels))))
    (thunk (send vm set reg (exec))
           (send vm step))))

(define (generate-op-expr vm expr labels)
  (let* ((proc (send vm op (cadar expr)))
         (execs (map (lambda (x)
                       (generate-val-expr vm x labels))
                     (cdr expr))))
    (thunk (apply proc (map run execs)))))

(define (generate-val-expr vm expr labels)
  (case (car expr)
    ('const (thunk (cadr expr)))
    ('reg (thunk (send vm get (cadr expr))))
    ('label (thunk (hash-ref labels (cadr expr))))))

(define (op-expr? expr)
  (eq? 'op (caar expr)))

(define (run exec) (exec))
