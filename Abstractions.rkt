(define (sum outer-func inner-func outer-next inner-next inner-count outer-count final)
  (if (> outer-count final)
      0
      (+ (outer-func inner-func outer-count inner-count final) (sum outer-func inner-func outer-next inner-next (inner-next inner-count) (outer-next outer-count) final))
      )
  )


(define (simpson-integral f a b n)
  
  (define h (/ (- b a) n))
  (define (inner-next a) (+ a h))
  (define (outer-next x) (+ x 1))
  
  (define (g e x y m)
    (cond ((= x 0) (e y))
          ((= x m) (e y))
          ((odd? x) (* 4 (e y)))
          ((even? x) (* 2 (e y)))
          )
  )
  (* (/ h 3) (sum g f outer-next inner-next a 0 n))
  )
(define (cube x) (* x x x))

(simpson-integral cube 1 6.3 100)



(define (iter-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))
        )
    )
  (iter a 0)
  )
(define (nextA x) (+ 2 x))
(define (inc x) (+ 1 x))
(iter-sum cube 3 nextA 9)

(define (product term next a b)
  (if (> a b)
      1
      (* (term a) (product term next (next a) b))
      )
  )
(define (identity x) (* 1 x))

(product identity inc 1 5)

(define (pi-computer n)
  (if (odd? n)
      (/ (+ n 1) (+ n 2))
      (/ (+ n 2) (+ n 1))
      )
  )


(* 4 (product pi-computer inc 1.0 4000.0))

(define (product-iter term next a b)
  (define (iter term next a b total)
    (if (> a b)
        total
        (iter term next (next a) b (* total (term a)))
        )
    )
  (iter term next a b a)
  )

(* 4 (product-iter pi-computer inc 1.0 2000.0))

(define (accumulate combiner identity term next a b)
  (if (> a b)
      identity
      (combiner (term a) (accumulate combiner identity term next (next a) b))
      )
  )

(define (multiply-two x y) (* x y))

(* 4 (accumulate multiply-two 1 pi-computer inc 1.0 2000.0))

(define (iter-accumulate combiner term next a b)
  (define (iter combiner term next a b total)
    (if (> a b)
        total
        (iter combiner term next (next a) b (combiner (term a) total))
        )
    )
  (iter combiner term next a b a)
  )

(* 4 (iter-accumulate multiply-two pi-computer inc 1.0 2000.0)
   )

(define (filter-accum combiner identity filter term next a b)
  (if (> a b)
      identity
      (if (filter a)
          (combiner (term a) (filter-accum combiner identity filter term next (next a) b))
          (filter-accum combiner identity filter term next (next a) b)
          )
      )
  )

(define (prime? x)
  (define (great-divide x n total)
    (if (= 1 n)
        total
        (if (= 0 (remainder x n))
            (great-divide x (- n 1) (+ total 1))
            (great-divide x (- n 1) total)
            )
        )
    )
  (cond ((= x 1) #f)
        ((= 0 (great-divide x (- x 1) 0)) #t)
        (else #f)
      )
  )

(prime? 4)
(prime? 1)
(prime? 16)
(prime? 17)


(define (square x) (* x x))

(filter-accum (lambda (x y) (+ x y)) 0 prime? square inc 1 10)