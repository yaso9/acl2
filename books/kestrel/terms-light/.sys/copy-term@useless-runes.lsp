(COPY-TERM
 (455 187 (:REWRITE DEFAULT-+-2))
 (263 187 (:REWRITE DEFAULT-+-1))
 (128 32 (:DEFINITION INTEGER-ABS))
 (128 16 (:DEFINITION LENGTH))
 (80 16 (:DEFINITION LEN))
 (53 38 (:REWRITE DEFAULT-<-2))
 (42 38 (:REWRITE DEFAULT-<-1))
 (32 32 (:REWRITE DEFAULT-UNARY-MINUS))
 (16 16 (:TYPE-PRESCRIPTION LEN))
 (16 16 (:REWRITE DEFAULT-REALPART))
 (16 16 (:REWRITE DEFAULT-NUMERATOR))
 (16 16 (:REWRITE DEFAULT-IMAGPART))
 (16 16 (:REWRITE DEFAULT-DENOMINATOR))
 (16 16 (:REWRITE DEFAULT-COERCE-2))
 (16 16 (:REWRITE DEFAULT-COERCE-1))
 (4 4 (:LINEAR ACL2-COUNT-CAR-CDR-LINEAR))
 (1 1 (:TYPE-PRESCRIPTION COPY-TERM))
 )
(FLAG-COPY-TERM
 (671 291 (:REWRITE DEFAULT-+-2))
 (408 291 (:REWRITE DEFAULT-+-1))
 (216 54 (:DEFINITION INTEGER-ABS))
 (216 27 (:DEFINITION LENGTH))
 (135 27 (:DEFINITION LEN))
 (82 63 (:REWRITE DEFAULT-<-2))
 (70 63 (:REWRITE DEFAULT-<-1))
 (54 54 (:REWRITE DEFAULT-UNARY-MINUS))
 (27 27 (:TYPE-PRESCRIPTION LEN))
 (27 27 (:REWRITE DEFAULT-REALPART))
 (27 27 (:REWRITE DEFAULT-NUMERATOR))
 (27 27 (:REWRITE DEFAULT-IMAGPART))
 (27 27 (:REWRITE DEFAULT-DENOMINATOR))
 (27 27 (:REWRITE DEFAULT-COERCE-2))
 (27 27 (:REWRITE DEFAULT-COERCE-1))
 (4 2 (:TYPE-PRESCRIPTION RETURN-LAST))
 (2 2 (:TYPE-PRESCRIPTION THROW-NONEXEC-ERROR))
 )
(FLAG::FLAG-EQUIV-LEMMA)
(FLAG-COPY-TERM-EQUIVALENCES)
(LEN-OF-COPY-TERMS
 (40 20 (:REWRITE DEFAULT-+-2))
 (36 35 (:REWRITE DEFAULT-CDR))
 (20 20 (:REWRITE DEFAULT-+-1))
 (12 12 (:REWRITE DEFAULT-CAR))
 (8 1 (:DEFINITION COPY-TERM))
 )
(FLAG-LEMMA-FOR-PSEUDO-TERMP-OF-COPY-TERM
 (285 281 (:REWRITE DEFAULT-CDR))
 (269 258 (:REWRITE DEFAULT-CAR))
 (90 45 (:REWRITE DEFAULT-+-2))
 (47 47 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (47 47 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 (45 45 (:REWRITE DEFAULT-+-1))
 )
(PSEUDO-TERMP-OF-COPY-TERM)
(PSEUDO-TERMP-OF-COPY-TERMS)
(PSEUDO-TERM-LISTP-OF-COPY-TERMS)
(COPY-TERM
 (72 72 (:REWRITE DEFAULT-CDR))
 (60 60 (:REWRITE DEFAULT-CAR))
 (60 12 (:DEFINITION LEN))
 (24 12 (:REWRITE DEFAULT-+-2))
 (12 12 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 (12 12 (:REWRITE DEFAULT-+-1))
 (12 4 (:DEFINITION SYMBOL-LISTP))
 (11 11 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (8 4 (:DEFINITION TRUE-LISTP))
 )
