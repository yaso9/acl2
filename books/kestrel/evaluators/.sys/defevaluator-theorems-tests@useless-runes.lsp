(APPLY-FOR-DEFEVALUATOR)
(MYEV)
(EVAL-LIST-KWOTE-LST)
(TRUE-LIST-FIX-EV-LST)
(EV-COMMUTES-CAR)
(EV-LST-COMMUTES-CDR)
(MYEV-OF-FNCALL-ARGS)
(MYEV-OF-VARIABLE)
(MYEV-OF-QUOTE)
(MYEV-OF-LAMBDA)
(MYEV-LIST-OF-ATOM)
(MYEV-LIST-OF-CONS)
(MYEV-OF-NONSYMBOL-ATOM)
(MYEV-OF-BAD-FNCALL)
(MYEV-OF-BINARY-*-CALL)
(MYEV-OF-LAMBDA-BETTER
 (19 13 (:REWRITE MYEV-OF-BINARY-*-CALL))
 (10 10 (:REWRITE MYEV-OF-QUOTE))
 (8 8 (:REWRITE MYEV-LIST-OF-CONS))
 (8 8 (:REWRITE MYEV-LIST-OF-ATOM))
 )
(MYEV-LIST-OF-APPEND
 (256 128 (:TYPE-PRESCRIPTION TRUE-LISTP-APPEND))
 (128 128 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (128 128 (:TYPE-PRESCRIPTION BINARY-APPEND))
 (13 10 (:REWRITE DEFAULT-CAR))
 (12 9 (:REWRITE DEFAULT-CDR))
 (3 2 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (2 2 (:REWRITE MYEV-OF-VARIABLE))
 (2 2 (:REWRITE MYEV-OF-QUOTE))
 (2 2 (:REWRITE MYEV-OF-BINARY-*-CALL))
 )
(LEN-OF-MYEV-LIST
 (14 7 (:REWRITE DEFAULT-+-2))
 (8 8 (:REWRITE DEFAULT-CDR))
 (7 7 (:REWRITE DEFAULT-+-1))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 1 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (1 1 (:REWRITE MYEV-OF-VARIABLE))
 (1 1 (:REWRITE MYEV-OF-QUOTE))
 (1 1 (:REWRITE MYEV-OF-BINARY-*-CALL))
 )
(MYEV-LIST-OF-TRUE-LIST_FIX
 (7 6 (:REWRITE DEFAULT-CAR))
 (6 5 (:REWRITE DEFAULT-CDR))
 (3 2 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (2 2 (:REWRITE MYEV-OF-VARIABLE))
 (2 2 (:REWRITE MYEV-OF-QUOTE))
 (2 2 (:REWRITE MYEV-OF-BINARY-*-CALL))
 )
(MYEV-OF-FNCALL-ARGS-BACK)
(MYEV-LIST-IFF
 (2 2 (:REWRITE MYEV-LIST-OF-ATOM))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 1 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (1 1 (:REWRITE MYEV-OF-VARIABLE))
 (1 1 (:REWRITE MYEV-OF-QUOTE))
 (1 1 (:REWRITE MYEV-OF-BINARY-*-CALL))
 (1 1 (:REWRITE DEFAULT-CDR))
 )
(FLAG-LEMMA-FOR-MYEV-OF-CONS-IRREL
 (268 247 (:REWRITE DEFAULT-CAR))
 (193 193 (:REWRITE DEFAULT-CDR))
 (90 18 (:DEFINITION LEN))
 (70 35 (:REWRITE UNION-EQUAL-WHEN-NOT-CONSP-ARG2-CHEAP))
 (70 35 (:REWRITE UNION-EQUAL-WHEN-NOT-CONSP-ARG1-CHEAP))
 (57 20 (:REWRITE MYEV-OF-BINARY-*-CALL))
 (52 20 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (42 42 (:TYPE-PRESCRIPTION LEN))
 (36 18 (:REWRITE DEFAULT-+-2))
 (32 8 (:DEFINITION KWOTE-LST))
 (24 24 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (24 24 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 (18 18 (:REWRITE DEFAULT-+-1))
 (18 6 (:DEFINITION SYMBOL-LISTP))
 (12 6 (:DEFINITION TRUE-LISTP))
 (8 8 (:DEFINITION KWOTE))
 (6 6 (:TYPE-PRESCRIPTION SYMBOL-LISTP))
 )
(MYEV-OF-CONS-IRREL)
(MYEV-LIST-OF-CONS-IRREL)
(FLAG-LEMMA-FOR-MYEV-OF-CONS-IRREL2
 (144 141 (:REWRITE DEFAULT-CAR))
 (109 109 (:REWRITE DEFAULT-CDR))
 (77 5 (:REWRITE MYEV-OF-CONS-IRREL))
 (45 9 (:DEFINITION LEN))
 (24 24 (:TYPE-PRESCRIPTION FREE-VARS-IN-TERMS))
 (21 21 (:TYPE-PRESCRIPTION LEN))
 (19 19 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 (19 12 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (18 12 (:REWRITE MYEV-OF-BINARY-*-CALL))
 (18 9 (:REWRITE DEFAULT-+-2))
 (17 17 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (16 16 (:TYPE-PRESCRIPTION FREE-VARS-IN-TERM))
 (11 11 (:TYPE-PRESCRIPTION KWOTE-LST))
 (9 9 (:REWRITE DEFAULT-+-1))
 (9 3 (:DEFINITION SYMBOL-LISTP))
 (8 2 (:DEFINITION KWOTE-LST))
 (6 3 (:DEFINITION TRUE-LISTP))
 (5 5 (:REWRITE FREE-VARS-IN-TERM-WHEN-NOT-CONSP-CHEAP))
 (3 3 (:TYPE-PRESCRIPTION SYMBOL-LISTP))
 (2 2 (:DEFINITION KWOTE))
 )
(MYEV-OF-CONS-IRREL2)
(MYEV-LIST-OF-CONS-IRREL2)
(MYEV-LIST-WHEN-SYMBOL-LISTP
 (33 33 (:REWRITE DEFAULT-CAR))
 (32 32 (:REWRITE DEFAULT-CDR))
 (25 5 (:DEFINITION ASSOC-EQUAL))
 (3 3 (:REWRITE LOOKUP-EQUAL-WHEN-NOT-CONSP-CHEAP))
 (3 3 (:REWRITE LOOKUP-EQUAL-WHEN-NOT-ASSOC-EQUAL-CHEAP))
 (1 1 (:REWRITE MYEV-OF-QUOTE))
 (1 1 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (1 1 (:REWRITE MYEV-OF-BINARY-*-CALL))
 )
(MYEV-OF-CDR-OF-ASSOC-EQUAL
 (99 90 (:REWRITE DEFAULT-CAR))
 (82 9 (:DEFINITION SYMBOL-LISTP))
 (64 57 (:REWRITE DEFAULT-CDR))
 (23 8 (:REWRITE MYEV-LIST-OF-ATOM))
 (16 10 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (10 10 (:REWRITE MYEV-OF-QUOTE))
 (10 10 (:REWRITE MYEV-OF-BINARY-*-CALL))
 )
(FLAG-LEMMA-FOR-EQUAL-OF-MYEV-AND-MYEV-WHEN-ALISTS-EQUIV-ON
 (723 696 (:REWRITE DEFAULT-CAR))
 (601 599 (:REWRITE DEFAULT-CDR))
 (430 86 (:DEFINITION ASSOC-EQUAL))
 (271 43 (:REWRITE EQUAL-OF-CDR-OF-ASSOC-EQUAL-AND-CDR-OF-ASSOC-EQUAL-WHEN-ALISTS-EQUIV-ON))
 (190 19 (:DEFINITION MEMBER-EQUAL))
 (176 34 (:DEFINITION SYMBOL-LISTP))
 (98 49 (:REWRITE DEFAULT-+-2))
 (84 42 (:REWRITE UNION-EQUAL-WHEN-NOT-CONSP-ARG2-CHEAP))
 (84 42 (:REWRITE UNION-EQUAL-WHEN-NOT-CONSP-ARG1-CHEAP))
 (63 26 (:REWRITE MYEV-OF-BINARY-*-CALL))
 (56 14 (:DEFINITION KWOTE-LST))
 (49 49 (:REWRITE DEFAULT-+-1))
 (48 30 (:REWRITE MYEV-LIST-OF-ATOM))
 (41 41 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 (37 37 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (30 6 (:DEFINITION PAIRLIS$))
 (14 14 (:DEFINITION KWOTE))
 )
(EQUAL-OF-MYEV-AND-MYEV-WHEN-ALISTS-EQUIV-ON)
(EQUAL-OF-MYEV-LIST-AND-MYEV-LIST-WHEN-ALISTS-EQUIV-ON)