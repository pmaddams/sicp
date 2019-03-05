#lang racket/base

; Exercise 4.69

(provide (all-defined-out))

(require racket/promise
         racket/stream)

(struct expr (type val))

(define table (make-hash))

(define (put k v) (hash-set! table k v))

(define (get k) (hash-ref table k))
