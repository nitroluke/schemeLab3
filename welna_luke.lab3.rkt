; Lab3 in CSCI305 by luke Welna
#lang racket
(define (member? e lst) ;member? function takes in an atom and a list
  (if (null? lst)       ; checks if the list is null
      #f                ;  if the list is null, evaluate to false
      (if(list? (car lst)) ; if its not null, check if the value you pulled out of the list is a list
         (member? e (car lst)) ; if it is a list, recursively call member with the values in that list and e
         (if (equal? e (car lst))   ; if it is not a list check if e and car lst are equal
            #t (member? e (cdr lst)))))) ; if they are, it is a member, otherwise recursively call member with the cdr of the list
         

(define (set? lst)  ; set? functino takes in a list and checks if it is a well formed list
  (if (null? lst)   ; checks if the lsit is null
      #t (if (member? (car lst)(cdr lst)) ; if it is, evaluates to true, if it is false check if the car of the list is a member of the rest of a list
             #f (set? (cdr lst)))))  ; if it is, it is not a well formed list and evaluates to false, otherwise recursively call set with the cdr of the list
             

(define (union lst1 lst2)  ; union function evaluates the union of the two lists
  (makeWell(append lst1 lst2))  ; the union is literally just combined lists with duplicates removed, which is exactly what this does.
  ; uses the helper function called makeWell, which makes a passed in list a well formed list
  )

(define (makeWell lst)  ; makeWell function takes in a list and evaluates the wellformed list of said list
  (if (null? lst)       ; checks if the lsit is null
      '() (if (member? (car lst) (cdr lst)) ; if it is, return the empty list. Otherwise check if the car of the list is in the cdr of the list
              (makeWell (cdr lst)) (cons (car lst) (makeWell(cdr lst))))))     ; if it is a member, ignore the first value and recursively call with the cdr of the list
              ; if it isnt a member cons the car of the list to the recursive call of the cdr of the list
              

(define (intersect lst1 lst2)  ; intersect function takes in two lists and evaluates to the intersection
    (makeWell(if(null? lst1)   ; checks if the list is null
     '() (if (member? (car lst1) lst2) ; if it is, evaluates to the empty list, othe wise checks if car lst1 is part of lst2
               (cons (car lst1)  (intersect(cdr lst1) lst2)) (intersect (cdr lst1)lst2)))))   ;if member: cons car of lst1 witht the recursive call of intersect of the cdr of lst1 with lst2
                                                                                              ; if not a member: ignore car lst1 value and recursively call the cdr of lst1 with lst2              
               

