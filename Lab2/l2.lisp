;Task1
(SETQ l '(1 2 3 4 5 6))

(DEFUN SUBLST (l id len)
    (COND
        ((NULL l) NIL)
        ((> id  0) (SUBLST (CDR l) (- id 1) len))
        ((NULL len) l)
        ((> len  0) (CONS (CAR l) (SUBLST (CDR l) id (- len 1))))
    )
)

(DEFUN REVERSE!(l &optional (n 0))
        (if (= n (LENGTH l))
                '()
                (APPEND (REVERSE (SUBLST l n ( - (LENGTH l) n)))
                      (PROGN
                            (INCF n)
                            (REVERSE! l n)
                      )
                )
        )
)

(PRINT (REVERSE! l))


; Task 2
(DEFUN INSERTION (l x) 
  (COND ((NULL l) (LIST x))  
        ((> (CAR l) x) (CONS x l)) 
        (T (CONS (CAR l) (INSERTION (CDR l) x))))
  ) 
        

(DEFUN ISORT (x &optional (s NIL))
  (COND ((NULL x) s)
        (T (ISORT (CDR x) (INSERTION s (CAR x)))))
  ) 
        

(DEFUN SHELLS (l gap finalst) 
  (COND ((NULL l) finalst)  
        (T (COND ((> gap (LENGTH l)) (APPEND finalst (ISORT l)))
  (T (APPEND finalst (ISORT (SUBSEQ l 0 gap)) (SHELLS (SUBSEQ l gap (LENGTH l)) gap finalst)))))))


(DEFUN SHELL_SORT (l gaps)
  (COND ((NULL (CDR gaps)) (SHELLS l (CAR gaps) '()))  
        (T (SHELL_SORT (SHELLS l (CAR gaps) '()) (CDR gaps)))))

(DEFUN SEDJVIK_GAPS ()  
  '(1 8 23 77 281 1073 4193 16577 65921 262913 1050113 4197377 16783361 
   67121153 268460033 1073790977 4295065601 17180065793 68719869953 274878693377 
   1099513200641 4398049656833 17592192335873 70368756760577))

(DEFUN SORT! (l)
  (SHELL_SORT l (SEDJVIK_GAPS ))) 


(print (SORT! '(4 2 8 3 1 5 6 23 45 87 54 23 98 2 1 9 0 5 3 3 3 3 3 3 3 3 3 3 3 3 4 5 6 45 32 98 76 89 98 65 45 32 12 16))) 


;Task 3
(SETQ l4 '(1 2 3 8 5 4 -100 10))
  
       
(DEFUN PUT_IN (l x)
   (COND ((NULL l) (LIST x))   
         ((> (CAR l) x) (CONS x l)) 
         (T (CONS (CAR l) (PUT_IN (CDR l) x))))
   )  

(DEFUN INSERT_SORT (x &optional (s NIL)) 
   (COND ((NULL x) s)                 
         (T (INSERT_SORT (CDR x) (PUT_IN s (CAR x)))))
   )
                                                          
        
(print (INSERT_SORT l4) )


;Task 4
(SETQ l1 '(1 4 7))
(SETQ l2 '(0 5 6))


(DEFUN MERGE_LISTS (l1 l2)
    (COND ((NULL l1) l2)
          ((NULL l2) l1)
          ((> (CAR l1) (CAR l2)) (CONS (CAR l2) (MERGE_LISTS l1 (CDR l2))))
          (T (CONS (CAR l1) (MERGE_LISTS (CDR l1) l2)))
     )
)

(PRINT (MERGE_LISTS l1 l2))


;Task5
(DEFUN EQLISTS (lst1 lst2)                         
   (COND ((ATOM lst1) (EQ lst1 lst2))
         ((AND (NULL lst1) (NULL lst2)) T)          
         ((OR (AND (NULL lst1) (not (NULL lst2)))   
               (AND (not (NULL lst1)) (NULL lst2))  
         (not (equal (CAR lst1) (CAR lst2)))) NIL)  
         (T  (EQLISTS (CDR lst1) (CDR lst2)))
    )
)      
 
(DEFUN SUBLIST (l1 l2 H)
  (COND
    ((< H 0) NIL) 
    ((NULL l2) NIL)
    ((AND (EQLISTs (CAR l2) l1) (= 0 H)) T)
    ((atom (CAR l2)) (SUBLIST l1 (CDR l2) H))
    (T (OR (SUBLIST l1 (CAR l2) (- H 1)) (SUBLIST l1 (CDR l2) H))))
  )
 
(PRINT (SUBLIST '(7) '((7) (1 (7))) 0))
 
(PRINT (SUBLIST '(7) '((7) (1 (7))) 2))
