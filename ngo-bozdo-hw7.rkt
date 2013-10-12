;; Students:  Hoang Ngo & Marilda Bozdo
;; Usernames: hmngo     & mbozdo
;; --------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: Provide a data definition for a person. 
;;		  Each person has a name, an email address, and a list of friends (persons).
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
        (set! NETWORK (cons (make-person name email INIT-FRIEND-LIST) NETWORK))
        (first NETWORK)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4: Use create-person to populate the social network network with at least 5 different persons. 
;;            (You should "define" these new persons so you can refer to them in later exercises.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(create-person "Nate"  "nate@wpi.edu")
(create-person "Julia" "julia@wpi.edu")
(create-person "Peter" "peter@wpi.edu")
(create-person "Erik"  "erik@wpi.edu")
(create-person "Luke"  "luke@wpi.edu")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5: Write a function list-names-in-network that doesn't consume anything and 
;;            produces a list of the names of all people in the network. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-names-in-network : -> ListOfString
;; Interp. return list of names of all people in the NETWORK

(define (list-names-in-network)
      (map person-name NETWORK))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 6: Write a function list-all-names that doesn't consume anything and 
;;            produces a list of the names of all people in the network 
;;            (same signature and purpose as the previous problem). 
;;            This time, you must use accumulator-style programming to solve the problem.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-all-names : -> ListOfString
;; Interp. return a list contains all names of people in the network

(define (list-all-names)
      (list-all-names-accum NETWORK empty))
      
      
;; list-all-names-accum: ListOfPerson ListOfString -> ListOfString
;; Interp. return a list contains all names of people in the NETWORK

(define (list-all-names-accum alop list-of-names)
      (cond 
            [(empty? alop) list-of-names]
            [(cons?  alop) (list-all-names-accum (rest alop) 
                                                 (cons (person-name (first alop))
                                                       list-of-names))]))

                                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 7: Write a function friend that consumes two persons, and 
;;            makes them each a friend of the other. 
;; (A person A is a friend of person B if person A's friend list contains B and vice versa.) 
;; You may assume that the given persons are not already friends, and that both persons exist in the network. 
;; Your function should return void.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------- MAIN FUNCTION -------------------------------------------------------
;; friend : person person -> void
;; Interp. make two given people friends
;; EFFECT: add each person into other's friend list

(define (friend p1 p2)
      (begin
            (add-to-friend-list p1 p2)
            (add-to-friend-list p2 p1)))
            
            
;; ------------------------------------------------ HELPER FUNCTIONS -----------------------------------------------------
;; add-to-friend-list : person person -> void
;; Interp. add a formal person into latter person's friend list

(define (add-to-friend-list added-person person)
      (set-person-friend-list! person (cons added-person 
                                            (person-friend-list person))))

            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 8: Write a function most-social that consumes nothing and 
;;                      returns the person with the most friends (resolve ties arbitrarily). 
;;                      Return "empty network" if the network doesn't contain any persons. 
;;            You may use the built-in Racket function length.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------- CONSTANTS ---------------------------------------------------------
(define DEFAULT-MOST-SOCIAL (make-person "bot" "bot@wpi.edu" empty))


;; ------------------------------------------------- MAIN FUNCTION -------------------------------------------------------
;; most-social : -> person
;; Interp. return person with the most friends
;;         return "empty network" if the network doesn't have any person

(define (most-social)
       (cond
            [(empty? NETWORK) (error "empty network")]
            [(cons?  NETWORK) (person-most-friends NETWORK DEFAULT-MOST-SOCIAL)])) 
      

;; ------------------------------------------------ HELPER FUNCTIONS -----------------------------------------------------
;; person-most-friends : ListOfPerson person -> person
;; Interp. return the person with the most friends
;;         return "empty network" is the network doesn't have any person

(define (person-most-friends alop most-friends)
      (cond
            [(empty? alop) most-friends]
            [(cons?  alop) (person-most-friends (rest alop)
                                              (if (> (length (person-friend-list (first alop))) 
                                                     (length (person-friend-list most-friends)))
                                                  (first alop)
                                                  most-friends))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 9: Write a function change-email that consumes the name of a person and a new email address and 
;;            changes that person's email address to the new one. 
;;            Of course, the change should also show up in all occurrences of the named person in any of the friends lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; change-email : string string -> void
;; Interp. change email of a given person to a new one
;; EFFECT: change email of one person in NETWORK

(define (change-email name new-email)
      (local [(define (find-person name alop)
                  (cond
                        [(empty? alop) (error "no one has this name!")]
                        [(cons?  alop) (if (string=? name (person-name (first alop)))
                                           (first alop)
                                           (find-person name (rest alop)))]))]
            (set-person-email! (find-person name NETWORK) new-email)))
