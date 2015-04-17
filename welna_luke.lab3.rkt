#lang racket
(define (member? e lst)
  (if (null? lst)
      #f 
      (if(list? (car lst))
         (member? e (car lst))
         (if (equal? e (car lst))   
            #t (member? e (cdr lst))
         )
       )
     )
  )

  

;(define (member? e lst)
;  (if (null? lst)
;      #f (if (equal? e (car lst))   
;            #t (member? e (cdr lst))
;            )
;      )
;  )

(define (set? lst)
  (if (null? lst)
      #t (if (member? (car lst)(cdr lst))
             #f (set? (cdr lst))  
             )     
      )
  )

(define (union lst1 lst2) 
  (makeWell(append lst1 lst2))
  )

(define (makeWell lst)  
  (if (null? lst)
      '() (if (member? (car lst) (cdr lst))
              (makeWell (cdr lst)) (cons (car lst) (makeWell(cdr lst)))      
              )
      )
  )

(define (intersectHack lst1 lst2 interList)
    (if(null? lst1)
     interList (if (member? (car lst1) lst2)
               (append (cons (car lst1) interList) (intersectHack(cdr lst1) lst2 interList))  ;if member
               (append interList (intersectHack (cdr lst1)lst2 interList))   ;if not member
         )  
     )
  )

(define (intersect lst1 lst2)
  (makeWell(intersectHack lst1 lst2 '() ))
  )

(define (append! lst atom)
  (if(null? lst)
     '(atom) (cons (lst atom))
     )
  
  )