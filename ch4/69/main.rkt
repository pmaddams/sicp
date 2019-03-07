#lang racket/base

; Exercise 4.69

(provide (all-defined-out))

(require racket/promise
         racket/stream
         (only-in racket (eval builtin-eval)))

(struct database (assertions rules) #:mutable)

(define db (database empty-stream empty-stream))

(define table (make-hash))

(define (put k v) (hash-set! table k v))

(define (get k) (hash-ref table k))

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

(define (eval query s)
  (let ((proc (get (cons (type query) 'eval))))
    (if proc
        (proc (body query) s)
        (simple-query query s))))

(define (simple-query query s)
  (stream-append-map
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query frame)
      (delay (apply-rules query frame))))
   s))

(define (and-query clauses s)
  (if (null? clauses)
      s
      (and-query (cdr clauses)
                 (eval (car clauses) s))))

(define (or-query clauses s)
  (if (null? clauses)
      empty-stream
      (interleave-delayed
       (eval (car clauses) s)
       (delay (or-query (cdr clauses) s)))))

(define (not-query query s)
  (stream-append-map
   (lambda (frame)
     (if (stream-empty? (eval (car query) (stream frame)))
         (stream frame)
         empty-stream))
   s))

(define (value expr s)
  (stream-append-map
   (lambda (frame)
     (if (execute (instantiate expr frame (lambda (v f) (error "undefined:" v))))
         (stream frame)
         empty-stream))
   s))

(define (true expr s) s)

(define (execute expr)
  (let ((proc (car expr))
        (args (cdr expr))
        (ns (make-base-namespace)))
    (apply (eval proc ns) args)))

(define (find-assertions pattern frame)
  (stream-append-map
   (lambda (datum)
     (check-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-assertion assertion query frame)
  (let ((frame* (match query assertion frame)))
    (if (void? frame*)
        empty-stream
        (stream frame*))))

(define (match pattern datum frame)
  (cond ((void? frame) (void))
        ((equal? pattern datum) frame)
        ((variable? pattern) (extend-if-consistent pattern datum frame))
        ((and (pair? pattern)
              (pair? datum))
         (let ((frame* (match (car pattern) (car datum) frame)))
           (match (cdr pattern) (cdr datum) frame*)))
        (else (void))))

(define (extend-if-consistent var datum frame)
  (let ((binding (assoc var frame)))
    (if binding
        (match (cdr binding) datum frame)
        (extend var datum frame))))

(define (apply-rules pattern frame)
  (stream-append-map
   (lambda (rule)
     (apply-rule rule pattern frame))
   (fetch-rules pattern frame)))

(define (apply-rule rule pattern frame)
  (let* ((rule* (rename-variables rule))
         (frame* (unify pattern (conclusion rule) frame)))
    (if (void? frame*)
        empty-stream
        (eval (rule-body rule*) (stream frame*)))))

(define (rename-variables rule)
  (let ((id (gensym)))
    (let walk ((expr rule))
      (cond ((variable? expr) (make-variable expr id))
            ((pair? expr)
             (cons (walk (car expr))
                   (walk (cdr expr))))
            (else expr)))))

(define (unify p1 p2 frame)
  (cond ((void? frame) (void))
        ((equal? p1 p2) frame)
        ((variable? p1) (extend-if-possible p1 p2 frame))
        ((variable? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2))
         (let ((frame* (unify (car p1) (car p2) frame)))
           (unify (cdr p1) (cdr p2) frame*)))
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

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (indexed-assertions pattern)
      (database-assertions db)))

(define (indexed-assertions pattern)
  (get-stream (cons (index-key pattern) 'assertion-stream)))

(define (get-stream k)
  (let ((s (get k)))
    (if s s empty-stream)))

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (indexed-rules pattern)
      (database-rules db)))

(define (indexed-rules pattern)
  (stream-append
   (get-stream (cons (index-key pattern) 'rule-stream))
   (get-stream '(? rule-stream))))

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
           (s (get-stream (cons k 'assertion-stream))))
      (put '(cons k 'assertion-stream) (stream-cons assertion s)))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (when (indexable? pattern)
      (let* ((k (index-key pattern))
             (s (get-stream (cons k 'rule-stream))))
        (put (cons k 'rule-stream) (stream-cons rule s))))))

(define (index-key pattern)
  (let ((key (car pattern)))
    (if (variable? key)
        '?
        key)))

(define (indexable? pattern)
  (or (symbol? (car pattern))
      (variable? (car pattern))))

(define (use-index? pattern)
  (symbol? (car pattern)))

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

(define (stream-append-map f s)
  (stream-flatten (stream-map f s)))

(define (stream-flatten s)
  (if (stream-empty? s)
      empty-stream
      (interleave-delayed
       (stream-first s)
       (delay (stream-flatten (stream-rest s))))))

(define (interleave-delayed s ds)
  (if (stream-empty? s)
      (force ds)
      (stream-cons (stream-first s)
                   (interleave-delayed
                    (force ds)
                    (delay (stream-rest s))))))

(define (stream-append-delayed s ds)
  (if (stream-empty? s)
      (force ds)
      (stream-cons (stream-first s)
                   (stream-append-delayed
                    (stream-rest s)
                    ds))))
