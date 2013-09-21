;; Students:  Hoang Ngo
;; Usernames: hmngo
;; ------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: Develop data definitions for Borrower and ListOfBorrower.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct borrower (name country kind-of-business requested-loan percentage-raised))
;; borrower is (make-borrower String String String Number Number)
;; interp. (make-borrower name country kind-of-business requested-loan percentage-raised) is a borrower waiting for loan with
;;         name              as the name of the borrower
;;         country           as the country the borrower is from
;;         kind-of-business  as the kind of business the funds are needed for
;;         requested-loan    as the amount of requested loan
;;         percentage-raised as the percentage raised so far


;; ListOfBorrower is one of
;;      - empty
;;      - (cons borrower ListOfBorrower)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4: Write a function count-by-sector that consumes a list of borrowers and 
;;  																												 the name of a type of business and 
;; produces the number of borrowers in the list whose loans are for the given kind of business.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5: Write a function find-by-country that consumes the name of a country and a list of borrowers and 
;; returns the list of borrowers who are from that country.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 7: Write a function funds-needed that consumes a list of borrowers and 
;; produces the total amount of money that these borrowers are still seeking 
;; (ie, the sum of the amounts requested but not yet raised across all borrowers).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
