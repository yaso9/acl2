(MAKE-LAMBDA-APPLICATION-SIMPLE)
(PSEUDO-TERMP-OF-MAKE-LAMBDA-APPLICATION-SIMPLE
 (135 111 (:REWRITE DEFAULT-CAR))
 (122 116 (:REWRITE DEFAULT-CDR))
 (106 7 (:DEFINITION PSEUDO-TERM-LISTP))
 (68 23 (:REWRITE PSEUDO-TERM-LISTP-WHEN-SYMBOL-LISTP-CHEAP-2))
 (66 6 (:DEFINITION SET-DIFFERENCE-EQUAL))
 (66 6 (:DEFINITION INTERSECTION-EQUAL))
 (54 2 (:REWRITE PSEUDO-TERM-LISTP-OF-SET-DIFFERENCE-EQUAL))
 (52 26 (:REWRITE DEFAULT-+-2))
 (48 12 (:DEFINITION MEMBER-EQUAL))
 (32 14 (:REWRITE FREE-VARS-IN-TERM-WHEN-QUOTEP))
 (28 26 (:REWRITE DEFAULT-+-1))
 (22 11 (:REWRITE PSEUDO-TERMP-OF-CAR-WHEN-PSEUDO-TERM-LISTP-CHEAP))
 (22 11 (:REWRITE PSEUDO-TERM-LISTP-OF-CDR-WHEN-PSEUDO-TERM-LISTP-CHEAP))
 (20 20 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 (17 17 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (14 14 (:REWRITE FREE-VARS-IN-TERM-WHEN-NOT-CONSP-CHEAP))
 (12 12 (:TYPE-PRESCRIPTION MEMBER-EQUAL))
 (12 12 (:REWRITE SYMBOL-LISTP-WHEN-SUBSETP-EQUAL-1))
 (12 6 (:DEFINITION QUOTEP))
 (12 4 (:REWRITE SYMBOLP-OF-CAR-WHEN-SYMBOL-LISTP-CHEAP))
 (9 6 (:REWRITE APPEND-WHEN-NOT-CONSP-ARG1-CHEAP))
 (8 4 (:DEFINITION TRUE-LISTP))
 (8 2 (:DEFINITION TRUE-LIST-FIX))
 (6 6 (:TYPE-PRESCRIPTION QUOTEP))
 )
