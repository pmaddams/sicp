#lang sicp

(define front-ptr car)

(define rear-ptr cdr)

(define set-front-ptr! set-car!)

(define set-rear-ptr! set-cdr!)

(define prev-ptr cadr)

(define next-ptr cddr)

(define (set-prev-ptr! item prev)
  (set-car! (cdr item) prev))

(define (set-next-ptr! item next)
  (set-cdr! (cdr item) next))

(define (empty-deque? d)
  (null? (front-ptr d)))

(define (make-deque)
  (cons '() '()))

(define (front-deque d)
  (if (empty-deque? d)
      (error "front-deque: empty deque:" d)
      (car (front-ptr d))))

(define (rear-deque d)
  (if (empty-deque? d)
      (error "rear-deque: empty deque:" d)
      (car (rear-ptr d))))

(define (insert-deque! d item)
  (let* ((node (cons item (cons '() '())))
         (init (lambda ()
                 (set-front-ptr! d node)
                 (set-rear-ptr! d node)
                 d))
         (front (lambda ()
                  (set-next-ptr! node (front-ptr d))
                  (set-prev-ptr! (front-ptr d) node)
                  (set-front-ptr! d node)
                  d))
         (rear (lambda ()
                 (set-prev-ptr! node (rear-ptr d))
                 (set-next-ptr! (rear-ptr d) node)
                 (set-rear-ptr! d node)
                 d))
         (dispatch (lambda (m)
                     (if (and (empty-deque? d)
                              (or (eq? m 'front)
                                  (eq? m 'rear)))
                         (init)
                         (case m
                           ('front (front))
                           ('rear (rear))
                           (else (error "insert-deque!: unknown method:" m)))))))
    dispatch))

(define (delete-deque! d)
  (let* ((front (lambda ()
                  (set-front-ptr! d (next-ptr (front-ptr d)))
                  (if (not (null? (front-ptr d)))
                      (set-prev-ptr! (front-ptr d) '()))
                  d))
         (rear (lambda ()
                 (set-rear-ptr! d (prev-ptr (rear-ptr d)))
                 (if (not (null? (rear-ptr d)))
                     (set-next-ptr! (rear-ptr d) '()))
                 d))
         (dispatch (lambda (m)
                     (if (empty-deque? d)
                         (error "delete-deque!: empty deque:" d)
                         (case m
                           ('front (front))
                           ('rear (rear))
                           (else (error "delete-deque!: unknown method:" m)))))))
    dispatch))

(define (front-insert-deque! d item)
  ((insert-deque! d item) 'front))

(define (rear-insert-deque! d item)
  ((insert-deque! d item) 'rear))

(define (front-delete-deque! d)
  ((delete-deque! d) 'front))

(define (rear-delete-deque! d)
  ((delete-deque! d) 'rear))

(define (print-deque d)
  (letrec ((p (lambda (node)
                (display (car node))
                (if (not (null? (next-ptr node)))
                    (begin (display " ")
                           (p (next-ptr node)))))))
    (display "(")
    (if (not (empty-deque? d))
        (p (front-ptr d)))
    (display ")")
    (newline)))

(let ((d (make-deque)))
  (rear-insert-deque! d 'c)
  (print-deque d)
  (rear-insert-deque! d 'd)
  (print-deque d)
  (rear-insert-deque! d 'e)
  (print-deque d)
  (front-insert-deque! d 'b)
  (print-deque d)
  (front-insert-deque! d 'a)
  (print-deque d)
  (rear-delete-deque! d)
  (print-deque d)
  (rear-delete-deque! d)
  (print-deque d)
  (front-delete-deque! d)
  (print-deque d)
  (front-delete-deque! d)
  (print-deque d)
  (front-delete-deque! d)
  (print-deque d))
;; (c)
;; (c d)
;; (c d e)
;; (b c d e)
;; (a b c d e)
;; (a b c d)
;; (a b c)
;; (b c)
;; (c)
;; ()