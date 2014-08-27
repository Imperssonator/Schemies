#lang racket
(define (my-map process . biglist)
  

  (define (my-simple-map proc items)
    (display items) (newline)
    (if (null? items)
        '()
        (cons (proc (car items))
              (my-simple-map proc (cdr items))
              )
        )
    )

  
  (define (all-the-cars f a-list)
    (display a-list) (newline)
                         (if (null? (cdr a-list))
                                    (car (car a-list))
                                    (f (car (car a-list)) (all-the-cars f (cdr a-list)))
                                    )
                             )
    )
  
  (define (map-maker f a-list)
    (display a-list) (newline)
    (if (null? a-list)
        '()
        (cons (all-the-cars f a-list))
        )
    )
  (map-maker process biglist)
  )

           
           
           
(define a '(1 2))
(define b '(3 4))
(define c (list a b))
(cons (+ (car (car c)) (car (car (cdr c)))) (cons (+ (car (cdr (car c))) (car (cdr (car (cdr c))))) '()))
(my-map + a b)