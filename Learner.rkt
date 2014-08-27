#lang racket

(define a (open-input-file "Republic.txt"))

(define (define-word h)
  (cons h '())
  )

(define-word (read a))
