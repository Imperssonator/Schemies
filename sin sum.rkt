(define (sin-sum x n)
  
  (define (inverse-factorial m)
  (/ 1 (factorial m))
  )
         
  (define (factorial n)
  (define (butler product counter max-count)
    (cond ((= max-count 0) 1)
          ((> counter max-count) product)
          (else (butler (* product counter) (+ counter 1) max-count))
        )
    )
  (if (and (integer? n) (not (< n 0)))
  (butler 1 1 n)
  "I pity the foo that tries to factorialize non-integers!"
  )
  )
  
  (define (x^n x n)
    (define (butler product counter max-count)
      (if (> counter max-count)
          product
          (butler (* product x) (+ 1 counter) max-count)
          )
      )
    (butler 1 1 n)
    )
  
  (define (sin-butler product counter max-count)
    
    (display product) (display "\n")
    
    (cond ((> counter max-count) product)
          
          ((even? counter) ;;take out evens
           (sin-butler product (+ 1 counter) max-count))
          
          ((and (odd? counter) (integer? (/ (+ 1 counter) 4))) ;; subtract odds 3, 7, 11, ...
           (sin-butler (- product (* (x^n x counter) (inverse-factorial counter))) (+ 1 counter) max-count))
          
          ((and (odd? counter) (not (integer? (/ (+ 1 counter) 4)))) ;; add odds 1, 5, 9, ...
           (sin-butler (+ product (* (x^n x counter) (inverse-factorial counter))) (+ 1 counter) max-count))
          )
    )
  (sin-butler 0 1 n)
  )