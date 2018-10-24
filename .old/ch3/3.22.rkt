#lang sicp

(define (displayln x)
  (display x)
  (newline))

(define (make-queue)
  (let* ((front-ptr '())
         (rear-ptr '())
         (empty? (lambda ()
                   (null? front-ptr)))
         (front (lambda ()
                  (if (empty?)
                      '()
                      (car front-ptr))))
         (insert! (lambda (item)
                    (let ((node (cons item '())))
                      (if (empty?)
                          (set! front-ptr node)
                          (set-cdr! rear-ptr node))
                      (set! rear-ptr node))))
         (delete! (lambda ()
                    (if (empty?)
                        (error "make-queue: delete!: empty queue")
                        (set! front-ptr (cdr front-ptr)))))
         (print (lambda ()
                  (displayln front-ptr)))
         (dispatch (lambda (m)
                     (case m
                       ('empty? empty?)
                       ('front front)
                       ('insert! insert!)
                       ('delete! delete!)
                       ('print print)
                       (else (error "queue: unknown method:" m))))))
    dispatch))

(define (empty-queue? q)
  ((q 'empty?)))

(define (front-queue q)
  ((q 'front)))

(define (insert-queue! q item)
  ((q 'insert!) item)
  q)

(define (delete-queue! q)
  ((q 'delete!))
  q)

(define (print-queue q)
  ((q 'print)))

(let ((q (make-queue)))
  (displayln (empty-queue? q))
  (insert-queue! q 'a)
  (print-queue q)
  (displayln (front-queue q))
  (insert-queue! q 'b)
  (print-queue q)
  (displayln (empty-queue? q))
  (delete-queue! q)
  (print-queue q)
  (displayln (front-queue q))
  (delete-queue! q)
  (print-queue q))
;; #t
;; (a)
;; a
;; (a b)
;; #f
;; (b)
;; b
;; ()