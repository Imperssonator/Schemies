(define (make-box length offset thickness)
  ;;capable of making a box (or line, where thickness or length are 1) of dimensions length (height) x thickness (width), offset by "offset" from the left
  
  (define box-string "")
  ;;initiates the string
  
  (define (vertical-butler i j k box-string length offset thickness)
    ;;this butler procedure initiates and terminates the string, adding a newline at the end of every line
    (if (= i length)
        (begin (string-append box-string "\n") (display box-string))
        (offset-butler i 1 1 (string-append box-string "\n") length offset thickness)
        )
    )
  
  (define (offset-butler i j k box-string length offset thickness)
    ;;adds spaces to the string until the offset is reached
    (if (> k offset)
        (vert-thick-butler i j k box-string length offset thickness)
        (offset-butler i j (+ 1 k) (string-append box-string " ") length offset thickness)
               )
        )

  (define (vert-thick-butler i j k box-string length offset thickness)
    ;;adds # symbols until thickness is reached, then returns to vertical butler
    (if (> j thickness)
        (vertical-butler (+ i 1) j k box-string length offset thickness)
        (vert-thick-butler i (+ j 1) k (string-append box-string "#") length offset thickness)
     )
   )
    
(vertical-butler 0 1 1 box-string length offset thickness)
  ;;vertical-butler counter starts at 0 for some reason.  I don't remember why anymore.  All I know is that I spent a while figuring out why the 'slope' function in subsequent procedures was off by one.
)

(define (make-vert-downdiag length offset init-space final-space vert-thick diag-thick)
  
  ;;this makes a vertical line on the left, and a diagonal on the right.  Initial and final space are the spaces between the vertical and diag at the top and bottom.  This way, the diag can have positive or negative (or infinite, for parallel verticals) slope.  The thickness of each can be varied independently, even down to zero, which means that this procedure is capable of making a vertical and a diag, a single vertical, or a single diag.
  
  (define space-slope (/ (- final-space init-space) (- length 1)))
  ;;(length-1) is because the vertical butler starts counting at 0.  I realize this is a flaw, in fact, if I went and switched all the counters to start at 1, I wouldn't need the "make-box" function, because this could make single verticals (or horizontals) of length 1 without causing a divide by 0 error.
  ;;basically the same as make box, moving from butler to butler, the only real advancement in this function is the 'space-butler', which adds spaces of varying length every line
  (define vert-downdiag-string "")
  
  
  (define (vertical-butler i j k l m vert-downdiag-string length offset init-space final-space vert-thick diag-thick)
    (if (= i length)
        (begin (string-append vert-downdiag-string "\n") (display vert-downdiag-string))
        (offset-butler i 1 1 1 1 (string-append vert-downdiag-string "\n") length offset init-space final-space vert-thick diag-thick)
        )
    )
  
  (define (offset-butler i j k l m vert-downdiag-string length offset init-space final-space vert-thick diag-thick)
    (if (> k offset)
        (vert-thick-butler i j k l m vert-downdiag-string length offset init-space final-space vert-thick diag-thick)
        (offset-butler i j (+ 1 k) l m (string-append vert-downdiag-string " ") length offset init-space final-space vert-thick diag-thick)
               )
        )

  (define (vert-thick-butler i j k l m vert-downdiag-string length offset init-space final-space vert-thick diag-thick)
    (if (> j vert-thick)
        (space-butler i j k l m vert-downdiag-string length offset init-space final-space vert-thick diag-thick)
        (vert-thick-butler i (+ j 1) k l m (string-append vert-downdiag-string "#") length offset init-space final-space vert-thick diag-thick)
     )
   )
    
  (define (space-butler i j k l m vert-downdiag-string length offset init-space final-space vert-thick diag-thick)
    (if (> l (+ init-space (* i space-slope)))
        (diag-thick-butler i j k l m vert-downdiag-string length offset init-space final-space vert-thick diag-thick)
        (space-butler i j k (+ l 1) m (string-append vert-downdiag-string " ") length offset init-space final-space vert-thick diag-thick)
        )
    )
  
  (define (diag-thick-butler i j k l m vert-downdiag-string length offset init-space final-space vert-thick diag-thick)
    (if (> m diag-thick)
        (vertical-butler (+ i 1) j k l m vert-downdiag-string length offset init-space final-space vert-thick diag-thick)
        (diag-thick-butler i j k l (+ m 1) (string-append vert-downdiag-string "#") length offset init-space final-space vert-thick diag-thick)
        )
   )
  
  (vertical-butler 0 1 1 1 1 vert-downdiag-string length offset init-space final-space vert-thick diag-thick)
 )



(define (make-diags length offset init-space final-space thickness)
  
  ;;Init and final space are defined here as half the distance between the top and bottom of the diags, respectively, as if it were two halves of the 'make-vert-downdiag' function
  
  (define space-slope (/ (- final-space init-space) (- length 1)))
  
  (define diags-string "")
  
  (define (vertical-butler i j k l m n diags-string length offset init-space final-space thickness)
    (if (= i length)
        (begin (string-append diags-string "\n") (display diags-string))
        (offset-butler i 1 1 1 1 1 (string-append diags-string "\n") length offset init-space final-space thickness)
        )
    )
    
  (define (offset-butler i j k l m n diags-string length offset init-space final-space thickness)
    (if (> k offset)
        (outer-space-butler i j k l m n diags-string length offset init-space final-space thickness)
        (offset-butler i j (+ 1 k) l m n (string-append diags-string " ") length offset init-space final-space thickness)
               )
        )
  
  (define (outer-space-butler i j k l m n diags-string length offset init-space final-space thickness)
    (cond 
          ((> final-space init-space)
           (if (> n (- final-space init-space (* i space-slope)))
               (first-thick-butler i j k l m n diags-string length offset init-space final-space thickness)
               (outer-space-butler i j k l m (+ 1 n) (string-append diags-string " ") length offset init-space final-space thickness)
               )
           )
          ((< final-space init-space)
                  (if (> n (* i (* -1 space-slope)))
                  (first-thick-butler i j k l m n diags-string length offset init-space final-space thickness)
                  (outer-space-butler i j k l m (+ 1 n) (string-append diags-string " ") length offset init-space final-space thickness)
                   )
                 )
          ((= final-space init-space)
           (first-thick-butler i j k l m n diags-string length offset init-space final-space thickness)
           )
          )
    )
  
  (define (first-thick-butler i j k l m n diags-string length offset init-space final-space thickness)
    (if (> j thickness)
        (space-butler i j k l m n diags-string length offset init-space final-space thickness)
        (first-thick-butler i (+ j 1) k l m n (string-append diags-string "#") length offset init-space final-space thickness)
               )
               )
  
  (define (space-butler i j k l m n diags-string length offset init-space final-space thickness)
    (cond ((> final-space init-space)
           (if (> l (* 2 (- final-space (- n 1))))
               (diag-thick-butler i j k l m n diags-string length offset init-space final-space thickness)
               (space-butler i j k (+ l 1) m n (string-append diags-string " ") length offset init-space final-space thickness)
               )
           )
          ((< final-space init-space)
           (if (> l (* 2 (- init-space (- n 1))))
               (diag-thick-butler i j k l m n diags-string length offset init-space final-space thickness)
               (space-butler i j k (+ l 1) m n (string-append diags-string " ") length offset init-space final-space thickness)
               )
           )
          ((= final-space init-space)
           (if (> l (* 2 final-space))
               (diag-thick-butler i j k l m n diags-string length offset init-space final-space thickness)
               (space-butler i j k (+ l 1) m n (string-append diags-string " ") length offset init-space final-space thickness)
               )
           )
          )
    )
  
        
  (define (diag-thick-butler i j k l m n diags-string length offset init-space final-space thickness)
    (if (> m thickness)
        (vertical-butler (+ i 1) j k l m n diags-string length offset init-space final-space thickness)
        (diag-thick-butler i j k l (+ m 1) n (string-append diags-string "#") length offset init-space final-space thickness)
               )
               )
  

  (vertical-butler 0 1 1 1 1 1 diags-string length offset init-space final-space thickness)
  )

(define (diag length offset init-space final-space thickness)
  
  ;;Init and final space are defined here as half the distance between the top and bottom of the diags, respectively
  
  (define space-slope (/ (- final-space init-space) (- length 1)))
  
  (define diags-string "")
  
  (define (vertical-butler i j k l m n diags-string length offset init-space final-space thickness)
    (if (= i length)
        (begin (string-append diags-string "\n") (display diags-string))
        (offset-butler i 1 1 1 1 1 (string-append diags-string "\n") length offset init-space final-space thickness)
        )
    )
    
  (define (offset-butler i j k l m n diags-string length offset init-space final-space thickness)
    (if (> k offset)
        (outer-space-butler i j k l m n diags-string length offset init-space final-space thickness);;)     
        (offset-butler i j (+ 1 k) l m n (string-append diags-string " ") length offset init-space final-space thickness)
               )
        )
  
  (define (outer-space-butler i j k l m n diags-string length offset init-space final-space thickness)
    (cond 
          ((> final-space init-space)
           (if (> n (- final-space init-space (* i space-slope)))
               (first-thick-butler i j k l m n diags-string length offset init-space final-space thickness)
               (outer-space-butler i j k l m (+ 1 n) (string-append diags-string " ") length offset init-space final-space thickness)
               )
           )
          ((< final-space init-space)
                  (if (> n (* i (* -1 space-slope)))
                  (first-thick-butler i j k l m n diags-string length offset init-space final-space thickness)
                  (outer-space-butler i j k l m (+ 1 n) (string-append diags-string " ") length offset init-space final-space thickness)
                   )
                 )
          ((= final-space init-space)
           (first-thick-butler i j k l m n diags-string length offset init-space final-space thickness)
           )
          )
    )
  
  (define (first-thick-butler i j k l m n diags-string length offset init-space final-space thickness)
    (if (> j thickness)
        (vertical-butler (+ i 1) j k l m n diags-string length offset init-space final-space thickness)
        (first-thick-butler i (+ j 1) k l m n (string-append diags-string "#") length offset init-space final-space thickness)
               )
               )

  (vertical-butler 0 1 1 1 1 1 diags-string length offset init-space final-space thickness)
  )


(define (A n)

(define (middle-width n)
  (if (integer? (/ (+ n 1) 3))
      (- (+ (* 2 (/ n 3)) (* 2 n) 6) 1)
      (+ (* 2 (/ n 3)) (* 2 n) 6)
      )
  )

  (define (A-butler n)
    (make-diags (+ n 2) (+ n 3) 0 (+ n 1) (+ (/ n 3) 1))
    (make-box 1 (+ n 2) (middle-width n))
    (make-diags (+ n 2) 0 (+ n 3) (+ (* 2 n) 4) (+ (/ n 3) 1))
    )
  (if (and (integer? n) (>= n 0))
      (A-butler n)
      (display "\nI've got a fever... and the only prescription... is more integers as arguments!\n")
      )

)

(A 0)
(A 1)
(A 2)
(A 3)
(A 4)
(A 5)
(A 6)
(A 7)
(A 8)
(A 3.5)

(display "\n")
(display "\n")

(define (B size)
  
(make-box 1 0 3)
(make-vert-downdiag 2 0 2 2 1 1)
(make-box 1 0 3)
(make-vert-downdiag 2 0 2 2 1 1)
(make-box 1 0 3)
  )

(B 1)

(define (C size)
  
  (make-box 1 2 4)
  (diag 2 0 0 1 1)
  (make-box 1 0 1)
  (diag 2 0 1 0 1)
  (make-box 1 2 4)
  
 )

(C 1)

(define (D size)
  
  (make-box 1 0 4)
  (make-vert-downdiag 2 0 3 4 1 1)
  (make-vert-downdiag 2 0 4 4 1 1)
  (make-vert-downdiag 2 0 4 3 1 1)
  (make-box 1 0 4)
  
  )

(D 1)

