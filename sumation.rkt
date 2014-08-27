(define (sum lo hi)
  (define (butler product counter)
  
  (if (= counter hi)
      product
      (butler (+ product (+ counter 1)) (+ counter 1))
      )
  )
(butler lo 1)
  )
(sum 1 4)
