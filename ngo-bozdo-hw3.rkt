;; Students: Hoang Ngo
;; Username: hmngo
;; ------------------------------

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

(define-struct hurricane (name category max-wind storm-velocity storm-destination))
;; A hurricane is (make-hurricane String Natural Number Number String)
;; interp. (make-hurricane name category max-wind storm-velocity storm-destination) is a hurricane with
;;         name as its name
;;         category as its category (1 to 5, inclusive)
;;         max-wind as its maximum winds in miles per hour
;;         storm-velocity as the storm's velocity
;;         storm-destination as the storm's heading

(define-struct thunderstorm (amount-rainfall max-wind-gust storm-velocity storm-destination)
;; A thunderstorm is (make-thunderstorm Number Number Number String)
;; interp. (make-thunderstorm amount-rainfall max-wind-gust storm-velocity storm-destination) is a thunderstorm that has
;;         amount--rainfall as its number of inches of rainfall
;;         max-wind-gust as its maximum wind gust in miles per hour
;;         storm-velocity as the storm's velocity in miles per hour
;;         storm-destination as the storm's heading

