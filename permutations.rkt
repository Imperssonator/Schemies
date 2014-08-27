

(define (nPermute3 n)
  
(define (count1 counter1)
  (if (> counter1 n)
      "fin."
      (count2 counter1 1)
  )
 )
(define (count2 counter1 counter2)
  (if (> counter2 n)
      (count1 (+ counter1 1))
      (count3 counter1 counter2 1)
      )
  )      
      
(define (count3 counter1 counter2 counter3)
  (if (> counter3 n)
      (count2 counter1 (+ counter2 1))
      (begin (display counter1) (display " ") (display counter2) (display " ") (display counter3) (display "\n")
             (count3 counter1 counter2 (+ 1 counter3))                           
             )
      )
  )
(count1 1)
  )
(nPermute3 4)
