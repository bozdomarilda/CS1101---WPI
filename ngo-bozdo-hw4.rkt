;; Students:  Hoang Ngo
;; Usernames: hmngo
;; ------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: Develop data definitions for Borrower and ListOfBorrower.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct borrower (name country kind-of-business requested-loan percentage-raised))
;; borrower is (make-borrower String String String Number Number)
;; interp. (make-borrower name country kind-of-business requested-loan percentage-raised) is a borrower 
;;         waiting for loan with
;;            name              as the name of the borrower
;;            country           as the country the borrower is from
;;            kind-of-business  as the kind of business the funds are needed for
;;            requested-loan    as the amount of requested loan
;;            percentage-raised as the percentage raised so far


;; ListOfBorrower is one of
;;      - empty
;;      - (cons borrower ListOfBorrower)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4: Write a function count-by-sector that consumes a list of borrowers and 
;;  																												 the name of a type of business and 
;; produces the number of borrowers in the list whose loans are for the given kind of business.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; count-by-sector : ListOfBorrower String -> Number
;; Purpose: consume a list of borrower and the kind of business then
;;          return the number of borrowers whose loans are the same as the kind of business

(check-expect (count-by-sector empty "cow") 0)
(check-expect (count-by-sector (cons (make-borrower "A" "NZ" "Bee" 100 .5) empty) "cow") 0)


(define (count-by-sector alob type-of-business)
  (cond
      [(empty? alob) 0]
      [(cons?  alob) (if (string=? (borrower-kind-of-business (first alob)) 
      														 type-of-business)
                         (+ 1 
                         		(count-by-sector (rest lob) type-of-business))
                         (count-by-sector (rest lob) type-of-business))]))
                         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5: Write a function find-by-country that consumes the name of a country and a list of borrowers and 
;; returns the list of borrowers who are from that country.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; find-by-country : String ListOfBorrower -> ListOfBorrower
;; Purpose: consume the name of country and a list of borrowers
;;          return the list of borrowers who are from that country

(check-expect (find-by-country "Laos" (cons))) !!!! write test cases

(define (find-by-country country alob)
	(cond
			[(empty? alob) empty]
			[(cons?  alob) (if (string=? (borrower-country (first alob))
																	 country)
													(cons (first alob) 
																(find-by-country country (rest alob)))
													(find-by-country country (rest alob)))]
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 7: Write a function funds-needed that consumes a list of borrowers and 
;; produces the total amount of money that these borrowers are still seeking 
;; (ie, the sum of the amounts requested but not yet raised across all borrowers).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; funds-needed : ListOfBorrower -> Number
;; Purpose: consume a list of borrowers and
;;					return the total money that these borrowers are seeking

;;;; Test cases !!!!!!

(define (funds-needed alob)
	(cond
			[(empty? alob) 0]
			[(cons?  alob) (+ (* (- 1 (borrower-percentage (first alob)))
													 (borrower-requested-loan (first alob)))
												(funds-needed (rest alob)))]))
