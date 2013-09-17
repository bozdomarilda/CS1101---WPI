;; Students:  Hoang Ngo & Marilda Bozdo
;; Usernames: hmngo     & mbozdo
;; --------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: A storm can be any one of the following:
;;
;;- hurricane: Information required for a hurricane consists of the name of the hurricane, 
;;             the category (a number between 1 and 5, inclusive), the maximum sustained winds in miles per hour, 
;;             the velocity of the storm in miles per hour, and the storm's heading (for example, NNW).
;;- thunderstorm: The definition of a thunderstorm consists of the number of inches of rainfall, 
;;                the maximum wind gust in miles per hour, the velocity of the storm in miles per hour, and its heading.
;;- fire: A fire is represented by the number of square miles it covers, the number of days it has been raging, 
;;        and the number of people displaced by the fire.
;;        
;;Develop a data definition for each type of storm described above, and a data definition for an itemization for storms. 
;;Provide at least one example for each kind of storm.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct hurricane (name category max-winds velocity heading))
;; Signature: a hurricane is a (make-hurricane string natural number number string)
;; Interp: (make-hurricane name category max-winds velocity heading) is a hurricane with
;;          name as the name of the hurricane
;;          category as a hurricane's category (a number between 1 and 5 inclusive)
;;          max-winds as the maximum sustained wind in miles per hour
;;          velocity as the velocity of the hurricane in miles per hour
;;          heading as the place where the hurricane is heading

(make-hurricane "Katrina" 5 175 150 "South Florida")

(define-struct thunderstorm (amount-rainfall max-wind-gust velocity heading))
;; Signature: a thunderstorm is a (make-thunderstorm number number number string)
;; Interp: (make-thunderstorm rainfall max-wind-gust velocity heading) is a thunderstorm with
;;           amount-rainfall as the number of inches of rainfall
;;           max-wind-gust as the maximum wind gust in miles per hour
;;           velocity as the velocity of the thunderstorm in miles per hour
;;           heading as the place where the thunderstorm is heading

(make-thunderstorm 5 50 80 "New York")

(define-struct fire (cover-area duration n-people-displaced))
;; Signature: a fire is a (make-fire number number number)
;; Interp: (make-fire cover-area duration n-people-displaced) is a fire with
;;          cover-area as the number of square miles it covers
;;          duration as the number of days it has been raging
;;          n-people-displaced as the number of people displaced by fire
(make-fire 100 10 50)


;; Storm is one of
;;      - hurricane
;;      - thunderstorm
;;      - fire
