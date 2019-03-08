#lang racket/base

; Exercise 4.69

(provide (all-defined-out))

(require racket/promise
         racket/stream
         (only-in racket (eval builtin-eval)))

(struct database (assertions rules) #:mutable)

(define db (database empty-stream empty-stream))

(define table (make-hash))

(define (put k1 k2 v)
  (hash-set! table (cons k1 k2) v))

(define (get k1 k2)
  (hash-ref table (cons k1 k2)))

(define counter 0)

(define (make-id)
  (let ((n counter))
    (set! counter (add1 n))
    n))

(define (type expr)
  (if (pair? expr)
      (car expr)
      (error "syntax error:" expr)))

(define (body expr)
  (if (pair? expr)
      (cdr expr)
      (error "syntax error:" expr)))

(define (instantiate expr frame fail)
  (let copy ((expr expr))
    (cond ((variable? expr)
           (let ((binding (assoc expr frame)))
             (if binding
                 (copy (cdr binding))
                 (fail expr frame))))
          ((pair? expr)
           (cons (copy (car expr))
                 (copy (cdr expr))))
          (else expr))))

(define (eval expr st)
  (let ((proc (get (type expr) 'eval)))
    (if proc
        (proc (body expr) st)
        (simple-query expr st))))

(define (simple-query expr st)
  (stream-append-map
   (lambda (frame)
     (stream-append-delayed
      (find-assertions expr frame)
      (delay (apply-rules expr frame))))
   st))

(define (and-query clauses st)
  (if (null? clauses)
      st
      (and-query (cdr clauses)
                 (eval (car clauses) st))))

(define (or-query clauses st)
  (if (null? clauses)
      empty-stream
      (interleave-delayed
       (eval (car clauses) st)
       (delay (or-query (cdr clauses) st)))))

(define (not-query clauses st)
  (stream-append-map
   (lambda (frame)
     (if (stream-empty? (eval (car clauses) (stream frame)))
         (stream frame)
         empty-stream))
   st))

(define (value expr st)
  (stream-append-map
   (lambda (frame)
     (if (execute (instantiate expr frame (lambda (v f) (error "undefined:" v))))
         (stream frame)
         empty-stream))
   st))

(define (true expr st) st)

(define (execute expr)
  (let ((proc (builtin-eval (car expr) (make-base-namespace)))
        (args (cdr expr)))
    (apply proc args)))

(define (find-assertions expr frame)
  (stream-append-map
   (lambda (datum)
     (check-assertion datum expr frame))
   (fetch-assertions expr frame)))

(define (check-assertion assertion expr frame)
  (let ((frame* (match expr assertion frame)))
    (if (void? frame*)
        empty-stream
        (stream frame*))))

(define (match expr datum frame)
  (cond ((void? frame) (void))
        ((equal? expr datum) frame)
        ((variable? expr) (extend-if-consistent expr datum frame))
        ((and (pair? expr)
              (pair? datum))
         (let ((frame* (match (car expr) (car datum) frame)))
           (match (cdr expr) (cdr datum) frame*)))
        (else (void))))

(define (extend-if-consistent var datum frame)
  (let ((binding (assoc var frame)))
    (if binding
        (match (cdr binding) datum frame)
        (extend var datum frame))))

(define (apply-rules expr frame)
  (stream-append-map
   (lambda (rule)
     (apply-rule rule expr frame))
   (fetch-rules expr frame)))

(define (apply-rule rule expr frame)
  (let* ((rule* (rename-vars rule))
         (frame* (unify expr (conclusion rule) frame)))
    (if (void? frame*)
        empty-stream
        (eval (rule-body rule*) (stream frame*)))))

(define (rename-vars rule)
  (let ((id (make-id)))
    (let walk ((expr rule))
      (cond ((variable? expr) (make-variable expr id))
            ((pair? expr)
             (cons (walk (car expr))
                   (walk (cdr expr))))
            (else expr)))))

(define (unify x1 x2 frame)
  (cond ((void? frame) (void))
        ((equal? x1 x2) frame)
        ((variable? x1) (extend-if-possible x1 x2 frame))
        ((variable? x2) (extend-if-possible x2 x1 frame))
        ((and (pair? x1) (pair? x2))
         (let ((frame* (unify (car x1) (car x2) frame)))
           (unify (cdr x1) (cdr x2) frame*)))
        (else (void))))

(define (extend-if-possible var val frame)
  (let ((binding (assoc var frame)))
    (if binding
        (unify (cdr binding) val frame)
        (if (variable? val)
            (let ((binding* (assoc val frame)))
              (if binding*
                  (unify var (cdr binding*) frame)
                  (extend var val frame)))
            (if (dependent? val var frame)
                (void)
                (extend var val frame))))))

(define (dependent? expr var frame)
  (let walk ((expr expr))
    (cond ((variable? expr)
           (if (equal? var expr)
               #t
               (let ((binding (assoc expr frame)))
                 (if binding
                     (walk (cdr binding))
                     #f))))
          ((pair? expr)
           (or (walk (car expr))
               (walk (cdr expr))))
          (else #f))))

(define (fetch-assertions expr frame)
  (if (use-index? expr)
      (indexed-assertions expr)
      (database-assertions db)))

(define (indexed-assertions expr)
  (get-stream (index-key expr) 'assertion-stream))

(define (get-stream k1 k2)
  (let ((st (get k1 k2)))
    (if st st empty-stream)))

(define (fetch-rules expr frame)
  (if (use-index? expr)
      (indexed-rules expr)
      (database-rules db)))

(define (indexed-rules expr)
  (stream-append
   (get-stream (index-key expr) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion assertion)
  (if (rule? assertion)
      (add-rule assertion)
      (add-assertion assertion)))

(define (add-assertion assertion)
  (store-assertion-in-index assertion)
  (set-database-assertions!
   db
   (stream-cons assertion (database-assertions db))))

(define (add-rule rule)
  (store-rule-in-index rule)
  (set-database-rules!
   db
   (stream-cons rule (database-rules db))))

(define (store-assertion-in-index assertion)
  (when (indexable? assertion)
    (let* ((k (index-key assertion))
           (st (get-stream k 'assertion-stream)))
      (put k 'assertion-stream (stream-cons assertion st)))))

(define (store-rule-in-index rule)
  (let ((expr (conclusion rule)))
    (when (indexable? expr)
      (let* ((k (index-key expr))
             (st (get-stream k 'rule-stream)))
        (put k 'rule-stream (stream-cons rule st))))))

(define (index-key expr)
  (let ((key (car expr)))
    (if (variable? key)
        '?
        key)))

(define (indexable? expr)
  (or (symbol? (car expr))
      (variable? (car expr))))

(define (use-index? expr)
  (symbol? (car expr)))

(define (extend var val frame)
  (cons (cons var val) frame))

(define (make-variable var id)
  (cons '? (cons id (cdr var))))

(define (variable? expr)
  (tagged-list? expr '?))

(define (rule? stmt)
  (tagged-list? stmt 'rule))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(true)
      (caddr rule)))

(define (tagged-list? x tag)
  (and (pair? x)
       (eq? tag (car x))))

(define (stream-append-map f st)
  (stream-flatten (stream-map f st)))

(define (stream-flatten st)
  (if (stream-empty? st)
      empty-stream
      (interleave-delayed
       (stream-first st)
       (delay (stream-flatten (stream-rest st))))))

(define (interleave-delayed st delayed-st)
  (if (stream-empty? st)
      (force delayed-st)
      (stream-cons (stream-first st)
                   (interleave-delayed
                    (force delayed-st)
                    (delay (stream-rest st))))))

(define (stream-append-delayed st delayed-st)
  (if (stream-empty? st)
      (force delayed-st)
      (stream-cons (stream-first st)
                   (stream-append-delayed
                    (stream-rest st)
                    delayed-st))))

(define (expand-vars expr)
  (let walk ((expr expr))
    (cond ((symbol? expr) (expand-var expr))
          ((pair? expr)
           (cons (walk (car expr))
                 (walk (cdr expr))))
          (else expr))))

(define (expand-var s)
  (let ((l (symbol->list s)))
    (if (eq? #\? (car l))
        (list '? (list->symbol (cdr l)))
        s)))

(define (contract-var var)
  (string->symbol
   (if (number? (cadr var))
       (format "?~a-~a" (caddr var) (cadr var))
       (format "?~a" (cadr var)))))

(define (symbol->list s)
  (string->list (symbol->string s)))

(define (list->symbol l)
  (string->symbol (list->string l)))
