#lang racket

;; Nils Persson

;; Utility Tags

(define railroad-data-tag (cons "type" "railroad-data"))

(define link-tag (cons "type" "link"))

(define station-tag (cons "type" "station"))

;; Utility functions

;; display+

(define (display+ string)
  (display string)
  (newline)
  )

;; Assoc-Store

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

;; Assoc-mem

(define (assoc-mem key a-list)
    (if (equal? key (car (car a-list)))
        (car (cdr (car a-list)))
        (assoc-mem key (cdr a-list))
        )
    )

;; Make list of lists

(define (make-list-of-lists . a-list)  ;; This function is used to initiate the list of associative lists.
  (define (list-helper a-list)
    ;;(display+ a-list)
    (if (null? a-list)
        '()
        (cons (car a-list) (list-helper (cdr a-list)))
        )
    )
  (if (null? (filter (lambda (x) (not (list? x))) a-list)) ;; It will only accept lists as its arguments
      (list-helper a-list)
      "make-list-of-lists requires lists as inputs"
      )
  )

;; Add to railroad

(define (add-to-railroad a-list railroad)
  ;;(display+ a-list)
  ;;(display+ railroad)
  (if (null? railroad)
      (cons a-list '())
      (cons a-list (add-to-railroad (car railroad) (cdr railroad)))
      )
  )

;; Make New Railroad

(define (make-new-railroad . tags-list) ;; the tags list, for now, should only contain the "year founded" value
  
  ;;(display+ tags-list)
  (define railroad-keys-list (list "founding-year")) ;; this list contains all of the keys that could be associated with a railroad.
  
  (define (new-helper keys-list tags-list)
    ;;(display+ tags-list)
    (if (null? tags-list) ;; the program stops when it has exhausted the user's list of tags for the railroad
        '()
        (assoc-store (car keys-list) (car tags-list) (new-helper (cdr keys-list) (cdr tags-list)))
        )
    )
  
  (if (<= (length tags-list) (length railroad-keys-list))
       (make-list-of-lists (assoc-store (car railroad-data-tag) (cdr railroad-data-tag) (new-helper railroad-keys-list tags-list)))
       "Make-new-railroad currently accepts a founding year as its only argument"
       )
  ;; The if statement ensures that the user didn't enter more tags than our program has keys for.  It then nests the associative list of the railroad data in a larger list, which can have other associative lists added to it.
  )

;; Add station

(define (add-station railroad . tags-list)
  (display+ railroad)
  
  (define station-keys-list (list "number" "x-coord" "y-coord"))
  
  (define (new-helper keys-list tags-list) ;; this helper constructs the associative list of station keys and tags
    ;;(display+ tags-list)
    (if (null? tags-list) ;; the program stops when it has exhausted the user's list of tags for the station
        '()
        (assoc-store (car keys-list) (car tags-list) (new-helper (cdr keys-list) (cdr tags-list)))
        )
    )
  
  (define station (assoc-store (car station-tag) (cdr station-tag) (new-helper station-keys-list tags-list)))
  (display+ station)
  
  (define (sort-stations station1 railroad)  ;; this will place the station in the correct position to have the stations sorted by number
    (display+ railroad)
    (if (= 0 (count-stations railroad))
        ;(begin (display "This one: ") (display+ (add-to-railroad station1 railroad))
               (add-to-railroad station1 railroad);)
        (cond ((= (assoc-mem "number" (car railroad)) (assoc-mem "number" station1))
               "This station number already exists")
              ((< (assoc-mem "number" station1) (assoc-mem "number" (car railroad)))
               (add-to-railroad station1 railroad))
              (else (add-to-railroad (car railroad) (sort-stations station1 (cdr railroad))))
              )
        )
    )
  
  (if (<= (length tags-list) (length station-keys-list))
      (sort-stations station railroad)
      "Add-station currently accepts number, x-coordinate, and y-coordinate as arguments"
      )
  )
      

;; Station?

(define (station? element)
  (equal? (assoc-mem "type" element) "station")
  )

;; Link?

(define (link? element)
  (equal? (assoc-mem "type" element) "link")
  )

;; Count stations

(define (count-stations railroad)
  (length (filter station? railroad))
  )

;; Count links

(define (count-links railroad)
  (length (filter link? railroad))
  )

;; Display Railroad

(define (display-railroad railroad)
  (define (display-station station)
    (display "Station Number: ") (display+ (assoc-mem "number" station))
    (display "Coordinates: ") (display (assoc-mem "x-coord" station)) (display ",") (display+ (assoc-mem "y-coord" station))
    (newline)
    )
  (define (display-link link)
    "what a link will display\n"
    (newline)
    )
  (define (display-railroad-data railroad-data)
    (display "Year Founded: ") (display+ (assoc-mem "founding-year" railroad-data))
    (newline)
    )
  (display+ "======")
  (newline)
  (define (display-helper railroad)
    (if (null? railroad)
        (display "======")
        (cond ((station? (car railroad)) (begin (display-station (car railroad)) (display-helper (cdr railroad))))
              ((link? (car railroad)) (begin (display-link (car railroad)) (display-helper (cdr railroad))))
              (else (begin (display-railroad-data (car railroad)) (display-helper (cdr railroad))))
              )
        )
    )
  (display-helper railroad)
  )
           

;; Test Railroads
(define railroad1 (make-new-railroad 1873))
(define railroad2 (add-station railroad1 2 3 4))
(display+ railroad2)
(define railroad3 (add-station railroad2 4 4 4))
(display+ railroad3)
(define railroad4 (add-station railroad3 3 0 0))
(display+ railroad4)
(display-railroad railroad4)
;(add-station railroad1 2 3 4)