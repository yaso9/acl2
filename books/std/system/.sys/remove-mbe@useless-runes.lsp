(REMOVE-MBE-LOGIC/EXEC
 (2031 883 (:REWRITE DEFAULT-+-2))
 (1217 883 (:REWRITE DEFAULT-+-1))
 (640 160 (:DEFINITION INTEGER-ABS))
 (640 80 (:DEFINITION LENGTH))
 (249 187 (:REWRITE DEFAULT-<-2))
 (205 187 (:REWRITE DEFAULT-<-1))
 (160 160 (:REWRITE DEFAULT-UNARY-MINUS))
 (80 80 (:REWRITE DEFAULT-REALPART))
 (80 80 (:REWRITE DEFAULT-NUMERATOR))
 (80 80 (:REWRITE DEFAULT-IMAGPART))
 (80 80 (:REWRITE DEFAULT-DENOMINATOR))
 (80 80 (:REWRITE DEFAULT-COERCE-2))
 (80 80 (:REWRITE DEFAULT-COERCE-1))
 (51 51 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 (42 42 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (30 10 (:DEFINITION SYMBOL-LISTP))
 (19 19 (:LINEAR ACL2-COUNT-CAR-CDR-LINEAR))
 )
(REMOVE-MBE-LOGIC/EXEC-FLAG
 (1260 574 (:REWRITE DEFAULT-+-2))
 (773 574 (:REWRITE DEFAULT-+-1))
 (432 108 (:DEFINITION INTEGER-ABS))
 (432 54 (:DEFINITION LENGTH))
 (270 54 (:DEFINITION LEN))
 (167 130 (:REWRITE DEFAULT-<-2))
 (146 130 (:REWRITE DEFAULT-<-1))
 (108 108 (:REWRITE DEFAULT-UNARY-MINUS))
 (54 54 (:TYPE-PRESCRIPTION LEN))
 (54 54 (:REWRITE DEFAULT-REALPART))
 (54 54 (:REWRITE DEFAULT-NUMERATOR))
 (54 54 (:REWRITE DEFAULT-IMAGPART))
 (54 54 (:REWRITE DEFAULT-DENOMINATOR))
 (54 54 (:REWRITE DEFAULT-COERCE-2))
 (54 54 (:REWRITE DEFAULT-COERCE-1))
 (4 2 (:TYPE-PRESCRIPTION RETURN-LAST))
 (2 2 (:TYPE-PRESCRIPTION THROW-NONEXEC-ERROR))
 )
(FLAG::FLAG-EQUIV-LEMMA)
(REMOVE-MBE-LOGIC/EXEC-FLAG-EQUIVALENCES)
(FLAG-LEMMA-FOR-RETURN-TYPE-OF-REMOVE-MBE-LOGIC/EXEC.NEW-TERM
 (1396 1383 (:REWRITE DEFAULT-CAR))
 (678 339 (:REWRITE DEFAULT-+-2))
 (339 339 (:REWRITE DEFAULT-+-1))
 (256 256 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 (255 255 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 )
(RETURN-TYPE-OF-REMOVE-MBE-LOGIC/EXEC.NEW-TERM)
(RETURN-TYPE-OF-REMOVE-MBE-LOGIC/EXEC-LST.NEW-TERMS)
(REMOVE-MBE-LOGIC)
(PSEUDO-TERMP-OF-REMOVE-MBE-LOGIC)
(REMOVE-MBE-EXEC)
(PSEUDO-TERMP-OF-REMOVE-MBE-EXEC)
