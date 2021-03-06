#lang racket/base

; Exercise 4.63

(provide (all-defined-out))

(require (only-in racket (eval eval*))
         racket/list
         racket/promise
         racket/stream)

(define genesis
  '((son Adam Cain)
    (son Cain Enoch)
    (son Enoch Irad)
    (son Irad Mehujael)
    (son Mehujael Methushael)
    (son Methushael Lamech)
    (wife Lamech Ada)
    (son Ada Jabal)
    (son Ada Jubal)

    (rule (father ?son ?father)
          (or (son ?father ?son)
              (and (son ?mother ?son)
                   (wife ?father ?mother))))

    (rule (grandfather ?grandson ?grandfather)
          (and (father ?father ?grandfather)
               (father ?grandson ?father)))))

(define (initialize data)
  (set! db (database empty-stream empty-stream))
  (set! index (make-hash))

  (put 'and 'eval eval-and)
  (put 'or 'eval eval-or)
  (put 'not 'eval eval-not)
  (put 'value 'eval value)
  (put 'true 'eval true)

  (for ((expr (in-list data)))
    (add-rule-or-fact (expand-vars expr))))

(define (query expr)
  (let ((x (expand-vars expr)))
    (sort (stream->list
           (stream-map (lambda (frame)
                         (instantiate x frame (lambda (v f) (contract-var v))))
                       (eval x (stream '()))))
          (lambda (l1 l2)
            (string<? (format "~a" l1)
                      (format "~a" l2))))))

(struct database (facts rules) #:mutable)

(define db (void))

(define index (void))

(define (get k1 k2)
  (let ((k (cons k1 k2)))
    (and (hash-has-key? index k)
         (hash-ref index k))))

(define (put k1 k2 v)
  (let ((k (cons k1 k2)))
    (hash-set! index k v)))

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
        (eval-query expr st))))

(define (eval-query expr st)
  (stream-append-map
   (lambda (frame)
     (stream-append-delayed
      (match-facts expr frame)
      (delay (apply-rules expr frame))))
   st))

(define (eval-and clauses st)
  (if (null? clauses)
      st
      (eval-and (cdr clauses)
                (eval (car clauses) st))))

(define (eval-or clauses st)
  (if (null? clauses)
      empty-stream
      (interleave-delayed
       (eval (car clauses) st)
       (delay (eval-or (cdr clauses) st)))))

(define (eval-not clauses st)
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
  (let ((op (eval* (car expr) (make-base-namespace)))
        (args (cdr expr)))
    (apply op args)))

(define (match-facts expr frame)
  (stream-append-map
   (lambda (dat)
     (match-fact expr dat frame))
   (get-facts expr frame)))

(define (match-fact expr fact frame)
  (let ((frame* (match expr fact frame)))
    (if (void? frame*)
        empty-stream
        (stream frame*))))

(define (match expr dat frame)
  (cond ((void? frame) (void))
        ((equal? expr dat) frame)
        ((variable? expr) (extend-if-consistent expr dat frame))
        ((and (pair? expr)
              (pair? dat))
         (let ((frame* (match (car expr) (car dat) frame)))
           (match (cdr expr) (cdr dat) frame*)))
        (else (void))))

(define (extend-if-consistent var dat frame)
  (let ((binding (assoc var frame)))
    (if binding
        (match (cdr binding) dat frame)
        (extend var dat frame))))

(define (apply-rules expr frame)
  (stream-append-map
   (lambda (rule)
     (apply-rule rule expr frame))
   (get-rules expr frame)))

(define (apply-rule rule expr frame)
  (let* ((rule* (rename-vars rule))
         (frame* (unify expr (conclusion rule*) frame)))
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

(define (get-facts expr frame)
  (if (use-index? expr)
      (indexed-facts expr)
      (database-facts db)))

(define (indexed-facts expr)
  (get-stream (key expr) 'facts))

(define (get-stream k1 k2)
  (let ((st (get k1 k2)))
    (if st st empty-stream)))

(define (get-rules expr frame)
  (if (use-index? expr)
      (indexed-rules expr)
      (database-rules db)))

(define (indexed-rules expr)
  (stream-append
   (get-stream (key expr) 'rules)
   (get-stream '? 'rules)))

(define (add-rule-or-fact expr)
  (if (rule? expr)
      (add-rule expr)
      (add-fact expr)))

(define (add-fact fact)
  (store-fact-in-index fact)
  (set-database-facts!
   db
   (stream-cons fact (database-facts db))))

(define (add-rule rule)
  (store-rule-in-index rule)
  (set-database-rules!
   db
   (stream-cons rule (database-rules db))))

(define (store-fact-in-index fact)
  (when (indexable? fact)
    (let* ((k (key fact))
           (st (get-stream k 'facts)))
      (put k 'facts (stream-cons fact st)))))

(define (store-rule-in-index rule)
  (let ((expr (conclusion rule)))
    (when (indexable? expr)
      (let* ((k (key expr))
             (st (get-stream k 'rules)))
        (put k 'rules (stream-cons rule st))))))

(define (key expr)
  (let ((k (car expr)))
    (if (variable? k)
        '?
        k)))

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
  (type? '? expr))

(define (rule-or-fact? expr)
  (or (rule? expr) (fact? expr)))

(define (rule? expr)
  (type? 'rule expr))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(true)
      (caddr rule)))

(define (type? t x)
  (and (pair? x)
       (eq? t (car x))))

(define (fact? expr)
  (let ((l (flatten expr)))
    (not (memq '? l))))

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

(define (stream-display st)
  (stream-for-each displayln st))

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
