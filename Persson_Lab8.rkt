
;; ====== LAB 8 ======
;;    Author(s):  Nils Persson
;;                
;;  Lab Section:  2:30


;;;; Utility Functions

(define (make-point x y z)
  (list x y z)
  )
(define (get-x p)
  (car p)
  )
(define (get-y p)
  (cadr p)
  )
(define (get-z p)
  (caddr p)
  )
(define inglorious (list (list 1 2 3) (list 2 5 3) (list -5 -3 5) (list .5 -30 22)))

(define (make-pt-list p pt-list)
  (define (helper pt-list)
    (if (null? pt-list)
        '()
        (cons (car pt-list) (helper (cdr pt-list)))
        )
    )
  (cons p (helper pt-list))
  )
(define the-empty-pt-list '())
(define (get-first-point pt-list) (car pt-list))
(define (get-rest-points pt-list) (cdr pt-list))
(define (sum-xcoord pt-list)
  (if (null? pt-list)
      0
      (+ (get-x (car pt-list)) (sum-xcoord (cdr pt-list)))
      )
  )
(define (max-xcoord pt-list)
  (define (helper pt-list themax)
    (if (null? pt-list)
        themax
        (helper (cdr pt-list) (max themax (get-x (car pt-list))))
        )
    )
  (cond ((null? pt-list) 0)
        ((null? (cdr pt-list)) (get-x (car pt-list)))
        (else (let ((firstmax (max (get-x (car pt-list)) (get-x (cadr pt-list)))))
                (helper (cddr pt-list) firstmax)
                )
              )
        )
  )

(define (distance p1 p2)
  (define (square x) (* x x))
  (sqrt (+ (square (- (get-x p1) (get-x p2))) (square (- (get-y p1) (get-y p2))) (square (- (get-z p1) (get-z p2)))))
  )

(display "--- Test Cases for Step 0, modified distance formula ---")
(newline)
(display "distance between (0 0 0) and (1 1 1)")
(newline)
(display (distance (list 0 0 0) (list 1 1 1)))

(define (max-distance p pt-list)
  (define (helper p pt-list themax)
    (if (null? pt-list)
        themax
        (helper p (cdr pt-list) (max themax (distance p (car pt-list))))
        )
    )
  (cond ((null? pt-list) 0)
        ((null? (cdr pt-list)) (distance p (car pt-list)))
        (else (let ((firstmax (max (distance p (car pt-list)) (distance p (cadr pt-list)))))
                (helper p (cddr pt-list) firstmax)
                )
              )
        )
  )

(define (max-range pt-list)
  (define (helper pt-list themax)
    (if (null? pt-list)
        themax
        (helper (cdr pt-list) (max themax (max-distance (car pt-list) pt-list)))
        )
    )
  (cond ((null? pt-list) 0)
        ((null? (cdr pt-list)) 0)
        (else (helper pt-list 0))
              )
  )

(define (make-sorted-pt-list p pt-list)
  (define (helper p pt-list)
    (if (null? pt-list)
        (cons p '())
        (if (<= (distance (make-point 0 0 0) p) (distance (make-point 0 0 0) (car pt-list)))
            (cons p (helper (car pt-list) (cdr pt-list)))
            (cons (car pt-list) (helper p (cdr pt-list)))
            )
        )
    )
  (cond ((null? pt-list) (make-pt-list p pt-list))
        (else (helper p pt-list))
        )
  
  )
;;(define sortie (list (list 1 1 1) (list 2 2 2) (list 3 3 3)))
;;(make-sorted-pt-list (make-point 2.5 2.5 2.5) sortie)
        


;; Reloads the current file.
(define (reload)
  (load "lab8.scm")  ; Change file name if copied to a new file.
)

;; Square
(define (square x) (* x x))


;;;; Test Case Values:
;;;;   These are to help simplify the test cases by reusing the lists.

;; Lists
(define test-list1 '(1 2 3 4 5 6 7 8 9))
(define test-list2 '(5))
(define test-list3 '(1 2))
(define test-list4 '())
(define test-list5 '(1 2 3 4))

;; Trees
(define test-tree1 '(1 (2 3) 4))
(define test-tree2 '((1 2 3 4)))
(define test-tree3 '(1 ((2) 3) (4)))
(define test-tree4 '((1 (2 (3 (4 (5)))))))
(define test-tree5 '(((((1) 2) 3) 4) 5))
(define test-tree6 '((((1 2) 3 4) 5 6) 7 8))


;;;; Test Case Code:
;;;;   This will handle execution of the test cases we've included below.
;;;;   To run test cases for a step, uncomment the (do-tests #) line.
;;;;   Note:  This code will run on MIT Scheme, but would have to be modified
;;;;          to work with other versions of Scheme. 
;;;;          Change #t to #f in the line below to use for Dr Scheme / STk.
;;;;          Behavior under Dr Scheme / STk is not tested.
(define (do-tests n)
  (let* ((in-mit-scheme #f)  ;; ** Change this value 
	 (tests-symbol 
	  (string->symbol 
	   (string-append "test-cases-step-" 
			  (number->string n))))

	 (test-cases 
	  (if in-mit-scheme 
	      (eval tests-symbol user-initial-environment)
	      (eval tests-symbol (interaction-environment))))

	 (display-string (string-append 
			  "\n--- Test Cases for Step "
			  (number->string n)
			  " ---\n")))

    (display display-string)

    (for-each 
     (lambda (x)
       (if (and (pair? x) (eq? (car x) 'define))
	   (if in-mit-scheme 
	       (eval x user-initial-environment) 
	       (eval x (interaction-environment)))
	   (begin 
	     (display x)
	     (newline)
	     (display (if in-mit-scheme 
			  (eval x user-initial-environment) 
			  (eval x (interaction-environment))))
	     (newline))))
     test-cases)))


;;;;
;;;; Step 1 - Skipping Over Elements
;;;;

;; get-tail

(define (get-tail lst index)
  (if (or (null? lst) (< index 1))
      lst
      (get-tail (cdr lst) (- index 1))
      )
  )


;;=============;;
;;  Test Code  ;;
;;=============;;
(define test-cases-step-1 
 '(
    (get-tail test-list1 0)
    (get-tail test-list1 6)
    (get-tail test-list1 8)
    (get-tail test-list1 9)
    (get-tail test-list1 10)
    (get-tail test-list2 0)
    (get-tail test-list2 1)
    (get-tail test-list2 2)
    (get-tail test-list3 0)
    (get-tail test-list3 1)
    (get-tail test-list3 2)
    (get-tail test-list4 0)
    (get-tail test-list4 5)
    (get-tail test-list5 -1)
  ))

(do-tests 1)



;;;;
;;;; Step 2 - Yippy Skippy
;;;;

;; skip
(define (skip lst)
  (define (helper lst)
    (if (null? (cdr lst))
        (car lst)
        (helper (cdr lst))
        )
    )
  (cond ((null? lst) '())
        ((not (list? lst)) lst)
        (else (helper lst))
        )
  )


;;=============;;
;;  Test Code  ;;
;;=============;;
(define test-cases-step-2 
 '(
    (skip test-list1)
    (skip test-list2)
    (skip test-list3)
    (skip test-list4)
    (skip test-list5)
  ))

(do-tests 2)




;;;;
;;;; Step 3 - Reverse
;;;;

;; reverse HAD TO CHANGE NAME OF REVERSE

(define (reverse1 lst)
  
  (define (amputate-list a-list) ;; returns a list without its last element
    (if (null? (cdr a-list))
        '()
        (cons (car a-list) (amputate-list (cdr a-list)))
        )
    )
  
  (define (helper b-list) ;; rips off the anti-car and passes down the anti-cdr
    (if (null? b-list)
        '()
        (cons (skip b-list) (helper (amputate-list b-list)))
        )
    )
  
  (cond ((null? lst) lst)
        ((list? lst) (helper lst))
        (else '())
        )
  )

;;=============;;
;;  Test Code  ;;
;;=============;;
(define test-cases-step-3 
 '(
    (reverse1 test-list1)
    (reverse1 test-list2)
    (reverse1 test-list3)
    (reverse1 test-list4)
    (reverse1 test-list5)
  ))

(do-tests 3)




;;;;
;;;; Step 4 - Accepting a Variable Number of Arguments
;;;;

;; getargs
(define (getargs . args) args)

;; howmanyargs?
(define (howmanyargs? . args)
  (define (helper args total)
    (if (null? args)
        total
        (if (pair? (car args))
            (helper (cdr args) (+ total 1))
            (helper (cdr args) total)
            )
        )
    )
  (helper args 0)
  )

;;=============;;
;;  Test Code  ;;
;;=============;;
(define test-cases-step-4
 '(
    (getargs)
    (getargs 1)
    (getargs 1 2)
    (getargs 1 2 3)
    (getargs 1 2 3 4)
    (getargs 1 (list 2 3) 4)

    (howmanyargs?)
    (howmanyargs? 1)
    (howmanyargs? (list 1))
    (howmanyargs? (list 1) 2)
    (howmanyargs? (cons 1 2) 3)
    (howmanyargs? (cons 1 2) (cons 3 4))
    (howmanyargs? (cons 1 2) (list 3 4))
    (howmanyargs? 1 (list 2 3) 4)
  ))

(do-tests 4)



;;;;
;;;; Step 5 - Mapping
;;;;

;; square-list using cons
(define (square-list-cons items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-cons (cdr items)))))


;; square-list using map
(define (square-list-map items)
  (map square items))



;;=============;;
;;  Test Code  ;;
;;=============;;
(define test-cases-step-5 
 '(
    (square-list-cons test-list1)
    (square-list-cons test-list2)
    (square-list-cons test-list3)
    (square-list-cons test-list4)
    (square-list-cons test-list5)
    (square-list-map test-list1)
    (square-list-map test-list2)
    (square-list-map test-list3)
    (square-list-map test-list4)
    (square-list-map test-list5)
  ))

(do-tests 5)




;;;;
;;;; Step 6 - Deep-Reverse
;;;;

;; deep-reverse
(define (deep-reverse lst)
  
  (define (amputate-list a-list) ;; returns a list without its last element
    (if (null? (cdr a-list))
        '()
        (cons (car a-list) (amputate-list (cdr a-list)))
        )
    )
  
  (define (helper b-list) ;; rips off the anti-car and passes down the anti-cdr
    (if (null? b-list)
        '()
        (cons (if (list? (skip b-list)) ;; checks if the anti-car is a list, then reverses it if necessary
                  (deep-reverse (skip b-list))
                  (skip b-list)
                  )
              (helper (amputate-list b-list)))
        )
    )
  
  (cond ((null? lst) lst)
        ((list? lst) (helper lst))
        (else '())
        )
  )

;;=============;;
;;  Test Code  ;;
;;=============;;
(define test-cases-step-6 
 '(
    (deep-reverse test-list1)
    (deep-reverse test-list2)
    (deep-reverse test-list3)
    (deep-reverse test-list4)
    (deep-reverse test-list5)
    (deep-reverse test-tree1)
    (deep-reverse test-tree2)
    (deep-reverse test-tree3)
    (deep-reverse test-tree4)
    (deep-reverse test-tree5)
    (deep-reverse test-tree6)
  ))

(do-tests 6)


;;;;
;;;; Step 7 - Using Accumulate
;;;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; map
(define (my-map proc sequence)
  (accumulate (lambda (x y) (cons (proc x) y)) '() sequence))

;; append
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

;; length
(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;=============;;
;;  Test Code  ;;
;;=============;;
(define test-cases-step-7 
 '(
    (my-map square test-list4)
    (my-map square test-list2)
    (my-map square test-list3)
    (my-map square test-list5)

    (my-append test-list4 test-list5)
    (my-append test-list5 test-list4)
    (my-append test-list3 test-list3)
    (my-append test-list2 test-list3)

    (my-length test-list1)
    (my-length test-list2)
    (my-length test-list3)
    (my-length test-list4)
    (my-length test-list5)
    (my-length test-tree1)
    (my-length test-tree2)
    (my-length test-tree3)
  ))

(do-tests 7)

