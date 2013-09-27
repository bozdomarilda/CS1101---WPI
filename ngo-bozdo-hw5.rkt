;; Students:  Hoang Ngo & Marilda Bozdo
;; Usernames: hmngo		& mbozdo
;; ----------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: Write the data definition(s) needed for this binary search tree.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Data definition for book

(define-struct book (title authors cost n-sold isbn book-list))
;; a book is (make-book String ListOfAuthor Number Number Number ListOfBook)
;; interp. (make-book title authors cost n-sold isbn book-list) is a book that has
;;        title     as the title of the book
;;        authors   as the list of authors of the book
;;        cost      as the cost of the book
;;        n-sold    as the number of copies sold
;;        book-list as the list of book next to this book


; Data definition for ListOfAuthor

;; ListOfAuthor is one of
;;      -  empty
;;      - (cons String ListOfAuthor)


; Data definition for ListOfBook

;; ListOfBook is one of
;;      - empty
;;      - (cons book ListOfBook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2: Provide an example of binary search tree containing at least 5 books. 
;;            Make sure you construct your example so that the items in the tree are 
;;            ordered according to the binary search tree property, on the ISBN number.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3: Write the template(s) for the data definition(s) in Problem 1.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Template for book

(define (fun-for-book abook)
    (... (book-title                   abook)
         (fun-for-author (book-authors abook))
         (book-cost                    abook)
         (book-n-sold                  abook)
         (book-isbn                    abook)
         (fun-for-lob (book-book-list  abook))))

       
; Template for ListOfAuthor

(define (fun-for-author loa)
    (cond
        [(empty? loa) (...)]
        [(cons?  loa) (... (first loa)
                           (fun-for-author (rest loa)))]))
                           
                           
; Template for ListOfBook

(define (fun-for-lob lob)
    (cond
        [(empty? lob) (...)]
        [(cons?  lob) (... (fun-for-book (first lob))
                           (fun-for-lob (rest lob)))]))
                           
                           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 6: Write a function author-of-book? which consumes a binary search tree,
;;                                                            an ISBN number, 
;;                                                            and the name of an author. 
;;            The function returns true if one of the authors of the book with the given ISBN number has the given name. 
;;            If a book with the given ISBN doesn't exist in the tree, the function should return false. 
;; Your function should be written efficiently, 
;; such that it performs as few comparisons as is necessary to find the correct ISBN number in the tree.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------------------------------------------- HELPER FUNCTIONS -------------------------------------------------------
;; is-isbn? : number book -> boolean
;; Purpose: return true if the book's isbn is the same as the given isbn
;;                 false otherwise

(define (is-isbn? isbn abook)
    (= isbn
       (book-isbn abook)))
      
      
;; has-author? : string book -> boolean
;; Purpose: return true if the book's list of authors has the given author
;;                 false otherwise

(define (has-author? author abook)
    (cond
        [(empty? loa) false]
        [(cons?  loa) (or (string=? author (first loa)
                          (has-author? (rest loa))))]))
                          
                          
;; -------------------------------------------------- MAIN FUNCTION ------------------------------------------------------
;; author-of-book? : ListOfBook number string -> boolean
;; Purpose: find the book that has the given isbn first, and return false if couldn't find. Then
;;          find the given author in the found book's list of authors, return true if found and false otherwise

(define (author-of-book? lob isbn author)
    (cond
        [(empty? lob) false]
        [(cons?  lob) (or (and (is-isbn?   isbn   (first lob)) 
                               (has-author? author (first lob)))
                          (author-of-book? (rest lob)))]))
