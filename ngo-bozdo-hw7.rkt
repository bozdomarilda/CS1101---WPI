;; Students:  Hoang Ngo & Marilda Bozdo
;; Usernames: hmngo			& mbozdo
;; --------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: Provide a data definition for a person. 
;;						Each person has a name, an email address, and a list of friends (persons).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a person is (make-person string string ListOfPerson)
(define-struct person (name email friend-list))


;; ListOfPerson is one of
;;      - empty
;;      - (cons person ListOfPerson)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2: Define a variable for a social network called network that will hold a list of persons 
;;            (it should initially be empty).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define NETWORK empty)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3: Write a function create-person that consumes a person's name and email address and 
;;                    returns a new person with the given name and email (and no friends). 
;;            The new person should also be added to the network. 
;;            You may assume the named person does not already exist in the network.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------- CONSTANTS ---------------------------------------------------------
(define INIT-FRIEND-LIST empty)


;; ------------------------------------------------- MAIN FUNCTION -------------------------------------------------------
;; create-person: string string -> person
;; Interp. consume a person's name and email
;;         return a new person
;; EFFECT: add a person with given info into NETWORK

(define (create-person name email)
      (begin
        (cons (make-person name email INIT-FRIEND-LIST) NETWORK)
        (make-person name email INIT-FRIEND-LIST)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4: Use create-person to populate the social network network with at least 5 different persons. 
;;            (You should "define" these new persons so you can refer to them in later exercises.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5: Write a function list-names-in-network that doesn't consume anything and 
;;            produces a list of the names of all people in the network. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-names-in-network : -> ListOfString
;; Interp. return list of names of all people in the NETWORK

(define (list-names-in-network)
      (map person-name NETWORK))
