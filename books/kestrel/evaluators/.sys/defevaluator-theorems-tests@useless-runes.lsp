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
 (186 93 (:TYPE-PRESCRIPTION TRUE-LISTP-APPEND))
 (93 93 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (93 93 (:TYPE-PRESCRIPTION BINARY-APPEND))
 (9 6 (:REWRITE DEFAULT-CAR))
 (8 5 (:REWRITE DEFAULT-CDR))
 (3 2 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (2 2 (:REWRITE MYEV-OF-VARIABLE))
 (2 2 (:REWRITE MYEV-OF-QUOTE))
 (2 2 (:REWRITE MYEV-OF-BINARY-*-CALL))
 )
(LEN-OF-MYEV-LIST
 (6 3 (:REWRITE DEFAULT-+-2))
 (4 4 (:REWRITE DEFAULT-CDR))
 (3 3 (:REWRITE DEFAULT-+-1))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 1 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (1 1 (:REWRITE MYEV-OF-VARIABLE))
 (1 1 (:REWRITE MYEV-OF-QUOTE))
 (1 1 (:REWRITE MYEV-OF-BINARY-*-CALL))
 )
(CDR-OF-MYEV-LIST
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 1 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (1 1 (:REWRITE MYEV-OF-VARIABLE))
 (1 1 (:REWRITE MYEV-OF-QUOTE))
 (1 1 (:REWRITE MYEV-OF-BINARY-*-CALL))
 )
(CAR-OF-MYEV-LIST
 (8 5 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (5 5 (:REWRITE MYEV-OF-QUOTE))
 (5 5 (:REWRITE MYEV-OF-BINARY-*-CALL))
 (1 1 (:REWRITE DEFAULT-CDR))
 )
(TRUE-LISTP-OF-MYEV-LIST-TYPE
 (5 1 (:DEFINITION TRUE-LISTP))
 (4 1 (:REWRITE CDR-OF-MYEV-LIST))
 (3 3 (:REWRITE MYEV-LIST-OF-ATOM))
 (3 3 (:REWRITE DEFAULT-CDR))
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
(MYEV-LIST-WHEN-QUOTE-LISTP
 (10 10 (:REWRITE DEFAULT-CDR))
 (1 1 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (1 1 (:REWRITE MYEV-OF-BINARY-*-CALL))
 )
(MYEV-LIST-OF-KWOTE-LST
 (69 6 (:DEFINITION QUOTE-LISTP))
 (25 23 (:REWRITE DEFAULT-CAR))
 (18 6 (:DEFINITION QUOTEP))
 (16 4 (:REWRITE MYEV-LIST-OF-ATOM))
 (14 12 (:REWRITE DEFAULT-CDR))
 (3 1 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (3 1 (:REWRITE MYEV-OF-BINARY-*-CALL))
 )
(MYEV-LIST-WHEN-SYMBOL-LISTP
 (35 5 (:DEFINITION QUOTE-LISTP))
 (34 34 (:REWRITE DEFAULT-CAR))
 (20 20 (:REWRITE DEFAULT-CDR))
 (20 4 (:DEFINITION ASSOC-EQUAL))
 (9 5 (:DEFINITION QUOTEP))
 (4 4 (:REWRITE MYEV-LIST-OF-ATOM))
 (3 3 (:REWRITE LOOKUP-EQUAL-WHEN-NOT-CONSP-CHEAP))
 (3 3 (:REWRITE LOOKUP-EQUAL-WHEN-NOT-ASSOC-EQUAL-CHEAP))
 (1 1 (:REWRITE MYEV-OF-QUOTE))
 (1 1 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (1 1 (:REWRITE MYEV-OF-BINARY-*-CALL))
 )
(MYEV-OF-FNCALL-ARGS-BACK)
(MYEV-LIST-IFF
 (33 3 (:REWRITE MYEV-LIST-WHEN-QUOTE-LISTP))
 (27 3 (:REWRITE MYEV-LIST-WHEN-SYMBOL-LISTP))
 (24 3 (:DEFINITION QUOTE-LISTP))
 (18 3 (:DEFINITION SYMBOL-LISTP))
 (15 15 (:TYPE-PRESCRIPTION SYMBOL-LISTP))
 (15 15 (:TYPE-PRESCRIPTION QUOTE-LISTP))
 (11 11 (:REWRITE DEFAULT-CAR))
 (7 7 (:REWRITE DEFAULT-CDR))
 (6 3 (:DEFINITION QUOTEP))
 (2 2 (:REWRITE MYEV-LIST-OF-ATOM))
 (2 1 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (1 1 (:REWRITE MYEV-OF-VARIABLE))
 (1 1 (:REWRITE MYEV-OF-QUOTE))
 (1 1 (:REWRITE MYEV-OF-BINARY-*-CALL))
 )
(FLAG-LEMMA-FOR-MYEV-OF-CONS-IRREL
 (680 80 (:DEFINITION QUOTE-LISTP))
 (671 52 (:REWRITE FREE-VARS-IN-TERMS-WHEN-QUOTE-LISTP))
 (527 506 (:REWRITE DEFAULT-CAR))
 (370 370 (:TYPE-PRESCRIPTION QUOTE-LISTP))
 (302 302 (:REWRITE DEFAULT-CDR))
 (288 8 (:DEFINITION KWOTE-LST))
 (219 28 (:REWRITE MYEV-LIST-WHEN-QUOTE-LISTP))
 (192 8 (:REWRITE CDR-OF-MYEV-LIST))
 (134 25 (:DEFINITION SYMBOL-LISTP))
 (90 18 (:DEFINITION LEN))
 (70 35 (:REWRITE UNION-EQUAL-WHEN-NOT-CONSP-ARG2-CHEAP))
 (70 35 (:REWRITE UNION-EQUAL-WHEN-NOT-CONSP-ARG1-CHEAP))
 (68 28 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (65 28 (:REWRITE MYEV-OF-BINARY-*-CALL))
 (64 8 (:REWRITE CAR-OF-MYEV-LIST))
 (42 42 (:TYPE-PRESCRIPTION LEN))
 (36 18 (:REWRITE DEFAULT-+-2))
 (26 26 (:REWRITE MYEV-LIST-OF-ATOM))
 (24 24 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (24 24 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 (18 18 (:REWRITE DEFAULT-+-1))
 (12 6 (:DEFINITION TRUE-LISTP))
 (8 8 (:DEFINITION KWOTE))
 )
(MYEV-OF-CONS-IRREL)
(MYEV-LIST-OF-CONS-IRREL)
(FLAG-LEMMA-FOR-MYEV-OF-CONS-IRREL2
 (198 195 (:REWRITE DEFAULT-CAR))
 (139 139 (:REWRITE DEFAULT-CDR))
 (136 16 (:DEFINITION QUOTE-LISTP))
 (105 14 (:REWRITE MYEV-LIST-WHEN-QUOTE-LISTP))
 (77 6 (:REWRITE FREE-VARS-IN-TERMS-WHEN-QUOTE-LISTP))
 (74 74 (:TYPE-PRESCRIPTION QUOTE-LISTP))
 (72 2 (:DEFINITION KWOTE-LST))
 (65 12 (:DEFINITION SYMBOL-LISTP))
 (48 2 (:REWRITE CDR-OF-MYEV-LIST))
 (45 9 (:DEFINITION LEN))
 (24 24 (:TYPE-PRESCRIPTION FREE-VARS-IN-TERMS))
 (23 14 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (21 21 (:TYPE-PRESCRIPTION LEN))
 (20 14 (:REWRITE MYEV-OF-BINARY-*-CALL))
 (19 19 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 (18 18 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (18 9 (:REWRITE DEFAULT-+-2))
 (16 2 (:REWRITE CAR-OF-MYEV-LIST))
 (12 12 (:TYPE-PRESCRIPTION FREE-VARS-IN-TERM))
 (12 12 (:REWRITE MYEV-LIST-OF-ATOM))
 (11 11 (:TYPE-PRESCRIPTION KWOTE-LST))
 (9 9 (:REWRITE DEFAULT-+-1))
 (6 6 (:REWRITE FREE-VARS-IN-TERMS-WHEN-NOT-CONSP-CHEAP))
 (6 3 (:DEFINITION TRUE-LISTP))
 (4 4 (:REWRITE FREE-VARS-IN-TERM-WHEN-NOT-CONSP-CHEAP))
 (2 2 (:DEFINITION KWOTE))
 )
(MYEV-OF-CONS-IRREL2)
(MYEV-LIST-OF-CONS-IRREL2)
(MYEV-OF-CDR-OF-ASSOC-EQUAL
 (234 6 (:REWRITE CDR-OF-MYEV-LIST))
 (225 16 (:REWRITE MYEV-LIST-WHEN-QUOTE-LISTP))
 (187 138 (:REWRITE DEFAULT-CAR))
 (185 15 (:DEFINITION QUOTE-LISTP))
 (150 6 (:REWRITE CAR-OF-MYEV-LIST))
 (130 15 (:DEFINITION SYMBOL-LISTP))
 (111 82 (:REWRITE DEFAULT-CDR))
 (69 69 (:TYPE-PRESCRIPTION QUOTE-LISTP))
 (64 16 (:REWRITE MYEV-OF-LAMBDA-BETTER))
 (47 14 (:REWRITE MYEV-LIST-OF-ATOM))
 (43 15 (:DEFINITION QUOTEP))
 (34 16 (:REWRITE MYEV-OF-QUOTE))
 (34 16 (:REWRITE MYEV-OF-BINARY-*-CALL))
 (4 4 (:TYPE-PRESCRIPTION TRUE-LISTP-OF-MYEV-LIST-TYPE))
 )
(FLAG-LEMMA-FOR-EQUAL-OF-MYEV-AND-MYEV-WHEN-ALISTS-EQUIV-ON
 (896 107 (:DEFINITION QUOTE-LISTP))
 (825 66 (:REWRITE FREE-VARS-IN-TERMS-WHEN-QUOTE-LISTP))
 (744 717 (:REWRITE DEFAULT-CAR))
 (506 504 (:REWRITE DEFAULT-CDR))
 (505 505 (:TYPE-PRESCRIPTION QUOTE-LISTP))
 (432 18 (:REWRITE CDR-OF-MYEV-LIST))
 (365 44 (:REWRITE MYEV-LIST-WHEN-QUOTE-LISTP))
 (360 10 (:DEFINITION KWOTE-LST))
 (252 48 (:DEFINITION SYMBOL-LISTP))
 (210 6 (:DEFINITION PAIRLIS$))
 (144 18 (:REWRITE CAR-OF-MYEV-LIST))
 (105 105 (:TYPE-PRESCRIPTION FREE-VARS-IN-TERMS))
 (98 49 (:REWRITE DEFAULT-+-2))
 (96 96 (:TYPE-PRESCRIPTION FREE-VARS-IN-TERM))
 (84 42 (:REWRITE UNION-EQUAL-WHEN-NOT-CONSP-ARG2-CHEAP))
 (84 42 (:REWRITE UNION-EQUAL-WHEN-NOT-CONSP-ARG1-CHEAP))
 (81 44 (:REWRITE MYEV-OF-BINARY-*-CALL))
 (70 2 (:DEFINITION TRUE-LIST-FIX))
 (49 49 (:REWRITE DEFAULT-+-1))
 (42 42 (:REWRITE MYEV-LIST-OF-ATOM))
 (41 41 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 (37 37 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (20 4 (:DEFINITION ASSOC-EQUAL))
 (10 10 (:DEFINITION KWOTE))
 )
(EQUAL-OF-MYEV-AND-MYEV-WHEN-ALISTS-EQUIV-ON)
(EQUAL-OF-MYEV-LIST-AND-MYEV-LIST-WHEN-ALISTS-EQUIV-ON)
