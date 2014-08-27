;; Author: Nils Persson

;; PROBLEM 1: Write your answers here as a comment

;; PROBLEM 2:
;; Part 1: Your function

(define (find-e k)
  
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
  
  (define (butler product counter max-count)
    (if (> counter max-count)
        product
        (butler (+ product (inverse-factorial counter)) (+ 1 counter) max-count)
        )
    )
    (butler 0 0 k)
    
  )
(find-e 2)
(find-e 3)
(find-e 5)
(find-e 10)



;; Part 2: How close is your result to e? (Write as a comment)
;; 2 + (2606501 / 3628800) = 2.7182818011
;; whereas e = 2.7182818284.
;; This result (for n = 10, the most accurate) is accurate to 8 sig figs.  It is off by 2.84e-8.

;; Part 3: Your function for sin(x)

;;We already have definitions for factorial and inverse factorial.  Let's use these.

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
    
    ;;(display product) (display "\n")
    
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

(sin-sum (/ 3.14159 4) 12)
(sin-sum -3 12)


;;--===================================--

;; PROBLEM 3:
(define (make-banner in-string  width height)    
  (define thickness 2)
  (define inset (floor (/ width 2)) )
  ;; Make a horizontal line
  (define (make-horizontal thick width the-char)
    (define one-horizontal (string-append (make-string width the-char) "\n"))
    (define (mh-helper thick thing)
      (if (= thick 1)           thing
          (string-append thing (mh-helper (- thick 1) thing))
          ))    
    (mh-helper thick one-horizontal)
    )
  ;; Make a vertical line
  (define (make-vertical thickness height inset in-char)
    (define one-vertical 
      (string-append (string-append (make-string inset #\ ) 
                                    (make-string thickness in-char)) "\n"))
    (define (mv-helper height thing)
      (if (= height 1) thing
          (string-append thing (mv-helper (- height 1) thing))
          )
      ) 
    (mv-helper height one-vertical) 
    )
  ;; Call the function that draws the right letter
  (define  (make-banner-character in-char)
    (display in-char)
    ;; You'll need to change this
    (cond    
        ( (char=? in-char #\I) (make-cap-I) )
        ( (char=? in-char #\J) (make-cap-J) )
        ( #t   "unknown character in input string")
      )     
    )
  ;; Make an I
  (define (make-cap-I)
    (display "inside makecapI ")
    (define top-or-bottom (make-horizontal thickness width #\* ))
    (define middle (make-vertical thickness height inset #\*))
    (string-append (string-append (string-append  top-or-bottom middle) top-or-bottom) "\n")
  )
  ;; Make a J
  (define (make-cap-J)
    (display "inside makecap J ")
    (define top (make-horizontal thickness width #\* ))
    (define bottom (make-horizontal thickness inset #\* ))
    (define middle (make-vertical thickness height inset #\*))
    (string-append (string-append (string-append  top middle) bottom) "\n")
  )
  ;; Add more letters here
  ;; ...
  ;; Make an entire string of characters
  (define (mb-helper in-string)
    (if (= (string-length in-string) 0)
        "\n\ndone"
        (string-append 
           (make-banner-character (string-ref in-string 0))
           (mb-helper (substring in-string 1 (string-length in-string)))
         )
    )
  )
  ;; defensive tests
  ;; what about zero length string?
  (if (and (string? in-string) (<= width 50) (<= height 75))
      (mb-helper in-string)
      "error in make-banner input"  )
  )

;; Code to run your make-banner
(define test1 (make-banner "IJIJ"  25 15) )
(display test1)

