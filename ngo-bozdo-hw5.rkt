;; Students:  Hoang Ngo & Marilda Bozdo
;; Usernames: hmngo		& mbozdo
;; ----------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: Write the data definition(s) needed for this binary search tree.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Data definition for binary-tree

;; A binary tree is either
;;      - 'unknown
;;      - (make-book title authors cost n-sold isbn ltbt rtbt) in which
;;            title   is a string
;;            authors is a string
;;            cost    is a number
;;            n-sold  is a number
;;            ltbt    is a binary tree
;;            rtbt    is a binary tree

; Data definition for book

(define-struct book (title authors cost n-sold isbn lfbt rtbt))
;; a book is (make-book String ListOfAuthor Number Number Number book-node book-node)
;; interp. (make-book title authors cost n-sold isbn ltbt rtbt) is a book that has
;;        title   as the title of the book
;;        authors as the list of authors of the book
;;        cost    as the cost of the book
;;        n-sold  as the number of copies sold
;;        ltbt    as a binary tree of book
;;        rtbt    as a binary tree of book


; Data definition for ListOfAuthor

;; ListOfAuthor is one of
;;      - empty
;;      - (cons String ListOfAuthor)


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
         (fun-for-treenode (book-book-s abook)
         (fun-for-treenode (book-book-b abook))))

       
; Template for ListOfAuthor

(define (fun-for-author loa)
    (cond
        [(empty? loa) (...)]
        [(cons?  loa) (... (first loa)
                           (fun-for-author (rest loa)))]))
                           
                           
; Template for TreeNode

(define (fun-for-treenode tn)
    (cond 
        [(symbol? tn) (...)]
        [(book?   tn) (... (book-title                    abook)
                           (fun-for-author (book-authors  abook))
                           (book-cost                     abook)
                           (book-n-sold                   abook)
                           (book-isbn                     abook)
                           (fun-for-treenode (book-book-s abook))
                           (fun-for-treenode (book-book-b abook)))]))
                           
                           
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
;; Purpose: find the book that has the given isbn first (take advantage of short-circuit evaluation), 
;;              and return false if couldn't find. If found, then
;;          find the given author in the book's list of authors, 
;;              return true if found and false otherwise

(define (author-of-book? lob isbn author)
    (cond
        [(empty? lob) false]
        [(cons?  lob) (or (and (is-isbn?   isbn   (first lob)) 
                               (has-author? author (first lob)))
                          (author-of-book? (rest lob)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 7: Write a function add-new-book. 
;;            The function consumes a binary search tree, an ISBN, title, a list of authors, and a price
;;                         and adds a new book with the given information to the binary search tree. 
;;            The new book has zero copies sold. Make sure that the tree that is produced is a binary search tree. 
;; You may assume that the ISBN number of the book to be added does not already exist in the given tree. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define INIT-COPIES-SOLD 0)

;; -------------------------------------------------- MAIN FUNCTION ------------------------------------------------------
;; add-new-book : book string ListOfAuthor Number -> book
;; Purpose: add a new book with given information to binary search tree

(define (add-new-book abook isbn title authors price)
        (    
        (insert title authors price INIT-COPIES-SOLD isbn (book-book-list abook))))
        
         
