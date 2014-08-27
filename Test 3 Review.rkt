#lang racket
(define (make-accumulator i)
  (lambda (add)
    (begin (set! i (+ i add)) i)))

(define B (make-accumulator 2))
B
(B 2)
(display (B 4))
(newline)

(define C (make-accumulator -20))
(C 4)
(B 4)

(define (make-monitored f)
  (let ((count 0))
    (lambda (input)
      (cond ((equal? input 'how-many-calls?)
             count)
            ((equal? input 'reset-counter)
             (set! count 0))
            (else (begin (set! count (+ count 1)) (f input)))))))

(define (square x) (* x x))
(define msquare (make-monitored square))
(msquare 2)
(msquare 10)
(msquare 'how-many-calls?)
(msquare 'reset-counter)
(msquare 1)
(msquare 'how-many-calls?)