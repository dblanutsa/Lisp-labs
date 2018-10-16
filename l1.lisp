(SETQ l1 '(Y U I))
(SETQ l2 '(G1 G2 G3))
(SETQ l3 '(KK LL MM JJJ))

;Task 1
(print ((LAMBDA (l1 l2 l3) 
                (CONS (CAR l1)
                      (CONS (CAR l2) 
                            (CONS (CAR l3)NIL)
                      )
                )
        )
l1 l2 l3))




;Task 2
(DEFUN LISTS_JOIN(l1 l2 l3)
           (LIST (NTH 1 l1)
                 (NTH 1 l2)
                 (NTH 2 l3)
           )
        
)

(PRINT (LISTS_JOIN l1 l2 l3))




; Task 3
(SETQ l1 '(5 U I))
(SETQ l2 '(G1 G2 G3))

(DEFUN WORK(l1 l2)
                    (IF (AND (NUMBERP (CAR l1))  
                             (> (CAR l1) -1)
                        )
                            l2
                            (CONS (FIRST l2) (REST l1))
                    )
       
        
)

(PRINT (WORk l1 l2))
