(CHARACTER-LISTP-OF-EXPLODE-ATOM+
 (71 37 (:REWRITE DEFAULT-CDR))
 (71 37 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE DEFAULT-REALPART))
 (2 2 (:REWRITE DEFAULT-IMAGPART))
 )
(OPEN-OUTPUT-CHANNEL-P1-OF-PRINC$-GEN
 (147 123 (:REWRITE DEFAULT-CAR))
 (145 29 (:DEFINITION ASSOC-EQUAL))
 (106 82 (:REWRITE DEFAULT-CDR))
 (40 16 (:DEFINITION BINARY-APPEND))
 (30 6 (:DEFINITION REVAPPEND))
 (15 15 (:TYPE-PRESCRIPTION REVAPPEND))
 (6 2 (:DEFINITION STRING-DOWNCASE1))
 (4 4 (:REWRITE DEFAULT-COERCE-2))
 (4 3 (:REWRITE DEFAULT-COERCE-1))
 (4 1 (:REWRITE DEFAULT-COERCE-3))
 (2 2 (:REWRITE DEFAULT-SYMBOL-NAME))
 (2 2 (:REWRITE DEFAULT-REALPART))
 (2 2 (:REWRITE DEFAULT-IMAGPART))
 )
(OPEN-OUTPUT-CHANNEL-P-OF-PRINC$)
(OPEN-OUTPUT-CHANNEL-ANY-P1-OF-PRINC$-GEN
 (953 797 (:REWRITE DEFAULT-CAR))
 (915 183 (:DEFINITION ASSOC-EQUAL))
 (710 554 (:REWRITE DEFAULT-CDR))
 (280 112 (:DEFINITION BINARY-APPEND))
 (210 42 (:DEFINITION REVAPPEND))
 (129 129 (:TYPE-PRESCRIPTION REVAPPEND))
 (44 11 (:REWRITE DEFAULT-COERCE-3))
 (42 14 (:DEFINITION STRING-DOWNCASE1))
 (36 36 (:REWRITE DEFAULT-COERCE-2))
 (36 25 (:REWRITE DEFAULT-COERCE-1))
 (14 14 (:REWRITE DEFAULT-SYMBOL-NAME))
 (14 14 (:REWRITE DEFAULT-REALPART))
 (14 14 (:REWRITE DEFAULT-IMAGPART))
 )
(OPEN-OUTPUT-CHANNEL-ANY-P-OF-PRINC$)
(STATE-P1-OF-PRINC$-GEN
 (115 115 (:REWRITE DEFAULT-CAR))
 (76 76 (:REWRITE DEFAULT-CDR))
 (40 16 (:DEFINITION BINARY-APPEND))
 (30 6 (:DEFINITION REVAPPEND))
 (15 5 (:REWRITE STATE-P-IMPLIES-AND-FORWARD-TO-STATE-P1))
 (10 10 (:TYPE-PRESCRIPTION STATE-P))
 (6 2 (:DEFINITION STRING-DOWNCASE1))
 (4 4 (:REWRITE DEFAULT-COERCE-2))
 (4 3 (:REWRITE DEFAULT-COERCE-1))
 (4 1 (:REWRITE DEFAULT-COERCE-3))
 (2 2 (:REWRITE DEFAULT-SYMBOL-NAME))
 (2 2 (:REWRITE DEFAULT-REALPART))
 (2 2 (:REWRITE DEFAULT-IMAGPART))
 )
(STATE-P-OF-PRINC$
 (6 2 (:REWRITE STATE-P-IMPLIES-AND-FORWARD-TO-STATE-P1))
 )
(W-OF-PRINC$
 (30 6 (:DEFINITION ASSOC-EQUAL))
 (28 28 (:REWRITE DEFAULT-CAR))
 (22 22 (:REWRITE DEFAULT-CDR))
 (20 8 (:DEFINITION BINARY-APPEND))
 (15 3 (:DEFINITION REVAPPEND))
 (6 6 (:TYPE-PRESCRIPTION STRING-DOWNCASE1))
 (6 6 (:TYPE-PRESCRIPTION EXPLODE-ATOM+))
 (4 4 (:TYPE-PRESCRIPTION TRUE-LISTP-EXPLODE-ATOM))
 (4 1 (:REWRITE DEFAULT-COERCE-3))
 (3 3 (:REWRITE DEFAULT-COERCE-2))
 (3 2 (:REWRITE DEFAULT-COERCE-1))
 (3 1 (:DEFINITION STRING-DOWNCASE1))
 (1 1 (:REWRITE DEFAULT-SYMBOL-NAME))
 (1 1 (:REWRITE DEFAULT-REALPART))
 (1 1 (:REWRITE DEFAULT-IMAGPART))
 )
