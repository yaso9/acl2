(UPPER-BOUND)
(UPPER-BOUND-POSITIVE-NATURAL)
(SELECTP)
(FAIR-SELECT
 (1 1 (:TYPE-PRESCRIPTION FAIR-SELECT))
 )
(FAIR-MEASURE)
(FAIR-STEP)
(FAIR-INV)
(FAIR-INIT)
(FAIR-MEASURE-NATURAL
 (10 6 (:REWRITE DEFAULT-<-1))
 (9 7 (:REWRITE DEFAULT-+-1))
 (8 7 (:REWRITE DEFAULT-+-2))
 (8 6 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE FOLD-CONSTS-IN-+))
 (2 2 (:REWRITE DEFAULT-UNARY-MINUS))
 )
(FAIR-MEASURE-DECREASES
 (80 64 (:REWRITE DEFAULT-+-2))
 (71 64 (:REWRITE DEFAULT-+-1))
 (47 29 (:REWRITE DEFAULT-<-2))
 (31 29 (:REWRITE DEFAULT-<-1))
 (19 15 (:REWRITE DEFAULT-UNARY-MINUS))
 (3 3 (:TYPE-PRESCRIPTION FAIR-SELECT))
 )
(FAIR-INV-IS-INVARIANT
 (6 3 (:REWRITE DEFAULT-<-2))
 (6 3 (:REWRITE DEFAULT-<-1))
 (1 1 (:REWRITE DEFAULT-+-2))
 (1 1 (:REWRITE DEFAULT-+-1))
 )
(FAIR-INV-OF-INIT)
(FAIR-RUN)
(FAIR-INV-OF-FAIR-RUN
 (4 4 (:REWRITE ZP-OPEN))
 (2 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:REWRITE DEFAULT-+-1))
 )
(LINEAR-FACTOID1)
(LINEAR-FACTOID2)
(FAIR-RUN-OF-1+
 (3 3 (:REWRITE DEFAULT-+-2))
 (3 3 (:REWRITE DEFAULT-+-1))
 (2 2 (:REWRITE ZP-OPEN))
 )
(ENV)
(ENV-MEASURE)
(ENV-MEASURE+-IS-NATURAL)
(ENV-MEASURE+-DECREASES
 (4 2 (:REWRITE DEFAULT-<-2))
 (3 2 (:REWRITE DEFAULT-<-1))
 (1 1 (:REWRITE DEFAULT-+-2))
 (1 1 (:REWRITE DEFAULT-+-1))
 )
