#lang racket

(define (display+ string)
  (display string)
  (newline)
  )
(display+ "++++++  HW2, Nils Persson, 3456499  ++++++")


;;I needed to add a lower and upper bound to this function. It does the range from lo to hi, including lo and hi.  I know this is not exactly what the problem asked for, but it is how the range function needs to work for subsequent problems.

(define (range lo hi)
  (define (helper lo hi the-list)
    (if (< hi lo)
        the-list
        (helper lo (- hi 1) (cons hi the-list))
        )
    )
  (if (and (integer? hi) (integer? lo))
      (helper lo hi '())
      "bounds must be positive integers!!!!"
      )
  )

(display+ "Problem 1: (range 6)")
(range 0 6)

(display+ "Problem 2: div357")

(define (div357? x)
  (or (= 0 (remainder x 3))
      (= 0 (remainder x 5))
      (= 0 (remainder x 7))
      )
  )
;;(div357? 21)

(define (sum-of-all-div357 lo hi)
  (let* ((range-list (range lo hi))
         (filtered-list (filter div357? range-list))
         )
    (foldl + 0 filtered-list)
    )
  )
(display "Using lists: ")
(sum-of-all-div357 1 100)

(define (sum-of-all-div357-nonlist lo hi)
  (if (< hi lo)
      0
      (if (div357? hi)
          (+ hi (sum-of-all-div357-nonlist lo (- hi 1)))
          (sum-of-all-div357-nonlist lo (- hi 1))
          )
      )
  )
(display "Without lists: ")
(sum-of-all-div357-nonlist 1 100)

(display+ "Problem 3: Max and Exists")

(define (max-loop a-list)
  
  (define (butler1 a-list)
    (define firstmax (max (car a-list) (car (cdr a-list))))
    
    (define (butler2 run-list themax)
      (if (null? run-list)
          themax
          (butler2 (cdr run-list) (max themax (car run-list)))
          )
      )
    (butler2 (cdr (cdr a-list)) firstmax)
    )
  (if (and (list? a-list) (>= (length a-list) 2))
      (butler1 a-list)
      "max-loop finds the max of a list of two or more members"
      )
  )

(max-loop '(3 5 23 65 2435 76 545 23 43 234534 643223 12 3 4 45 56 4 424 1 -44 .552345 .2 343))
(max-loop '(2 2))
(max-loop '(1))
          
(define (max-noloop a-list)
  (define (butler a-list)
    (define firstmax (max (car a-list) (car (cdr a-list))))
    (foldr max firstmax a-list)
    )
  (if (and (list? a-list) (>= (length a-list) 2))
      (butler a-list)
      "max-noloop finds the max of a list of two or more members"
      )
  )

(max-noloop '(3 5 23 65 2435 76 545 23 43 234534 643223 12 3 4 45 56 4 424 1 -44 .552345 .2 343))
(max-noloop '(2 2))
(max-noloop '(1))

(define (exist?-loop f a-list)
  (if (null? a-list)
      #f
      (if (f (car a-list))
          #t
          (exist?-loop f (cdr a-list))
          )
      )
  )
(exist?-loop div357? '(1 2 4 6 7))
(exist?-loop odd? '(2 4 6 8 10))

(define (exist?-noloop f a-list)
  (if (null? (filter f a-list))
      #f
      #t
      )
  )
(exist?-noloop div357? '(1 2 4 6 7))
(exist?-noloop odd? '(2 4 6 8 100))
      
(display+ "Problem 4: Optimus Primes")

(define (prime?-loop x)
  (define (butler x y)
    (if (< y 2)
        #t
        (if (= 0 (remainder x y))
            #f
            (butler x (- y 1))
            )
        )
    )
  (cond ((= x 1) #f)
        ((and (>= x 2) (integer? x))
         (butler x (- x 1))
         )
        (else #f)
        )
  )

;;(prime?-loop 1)
;;(prime?-loop 2)
;;(prime?-loop 3)
;;(prime?-loop -.34)

(define (prime?-noloop x)
  (cond ((= x 1) #f)
        ((= x 2) #t)
        ((and (> x 2) (integer? x))
         (if (null? (filter integer? (map (lambda (i) (/ x i)) (range 2 (- x 1)))))
             #t
             #f)
         )
        (else #f)
        )
  )
;;(prime?-noloop 1)
;;(prime?-noloop -.34)
;;(prime?-noloop 17)
;;(prime?-noloop 2)

(define (prime-sum-loop lo hi)
  (if (< hi lo)
      0
      (if (prime?-noloop hi)
          (+ hi (prime-sum-loop lo (- hi 1)))
          (prime-sum-loop lo (- hi 1))
          )
      )
  )

(display "Sum of primes between 1 and 20 using loop: ")
(prime-sum-loop 1 20)

(define (prime-sum-lists lo hi)
  (foldr + 0 (filter prime?-noloop (range lo hi)))
  )

(display "Sum of primes between 1 and 20 using lists: ")
(prime-sum-lists 1 20)

(display+ "Problem 5: Associated Lists")

(define (assoc-store key value a-list)
  
  ;;(display key) (newline) (display value) (newline)
  
  (define (butler top bottom)
    
    ;;(display+ top) (display+ bottom)
    
    (if (null? bottom)
        (cons top '())
        (cons top (butler (car bottom) (cdr bottom)))
        )
    )
  (butler (list key value) a-list)
  )

(define firstassoc (assoc-store "firstname" "john" '()))
firstassoc
(define secondassoc (assoc-store "lastname" "carlis" firstassoc))
secondassoc
(define thirdassoc (assoc-store "middlename" "DANGER ZONE" secondassoc))
thirdassoc

(define (assoc-keys a-list)
  (map car a-list)
  )

(assoc-keys thirdassoc)

(define (assoc-values a-list)
  (map cdr a-list)
  )

(assoc-values thirdassoc)

(define (assoc-mem key a-list)
    (if (equal? key (car (car a-list)))
        (car (cdr (car a-list)))
        (assoc-mem key (cdr a-list))
        )
    )
(assoc-mem "middlename" thirdassoc)

(define biglist

'((("firstname" "alice") ("hometown" "minneapolis") ("age" 25))

(("firstname" "bob") ("hometown" "duluth") ("age" 37))

(("firstname" "charlie") ("hometown" "edina") ("age" 22))

(("firstname" "dave") ("hometown" "minneapolis") ("age" 12))

(("firstname" "eve") ("hometown" "st paul") ("age" 39))

(("firstname" "frank") ("hometown" "minneapolis") ("age" 43))))

(display+ "Answers to Problem 5, Part 5:")

(map (lambda (a-list) (assoc-mem "hometown" a-list)) (filter (lambda (a-list) (> (assoc-mem "age" a-list) 30)) biglist))

(map (lambda (a-list) (assoc-mem "age" a-list)) (filter (lambda (a-list) (equal? (assoc-mem "hometown" a-list) "minneapolis")) biglist))

(foldr + 0 (map (lambda (a-list) (assoc-mem "age" a-list)) biglist))