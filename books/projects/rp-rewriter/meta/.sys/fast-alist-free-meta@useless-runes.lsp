(RP::FAST-ALIST-FREE-META)
(RP::RP-EVL-EXPANDER)
(RP::RP-EVL-EXPANDER-CORRECT)
(RP::FAST-ALIST-FREE-META-FORMULA-CHECKS)
(RP::META-EXTRACT-FORMULA-FAST-ALIST-FREE-WHEN-FAST-ALIST-FREE-META-FORMULA-CHECKS)
(RP::RP-EVL-OF-FAST-ALIST-FREE-WHEN-FAST-ALIST-FREE-META-FORMULA-CHECKS
 (16 13 (:REWRITE RP::RP-EVL-OF-QUOTE))
 (16 13 (:REWRITE RP::RP-EVL-OF-LAMBDA))
 )
(RP::VALID-SC-FAST-ALIST-FREE-RP-META
 (818 734 (:REWRITE DEFAULT-CDR))
 (807 639 (:REWRITE DEFAULT-CAR))
 (374 41 (:DEFINITION RP::INCLUDE-FNC-FN))
 (342 342 (:TYPE-PRESCRIPTION RP::TRANS-LIST))
 (327 25 (:REWRITE RP::RP-TRANS-IS-TERM-WHEN-LIST-IS-ABSENT))
 (310 52 (:REWRITE RP::VALID-SC-CADR))
 (241 39 (:REWRITE RP::ATOM-RP-TERMP-IS-SYMBOLP))
 (234 2 (:DEFINITION RP::EVAL-AND-ALL))
 (180 10 (:DEFINITION RP::RP-TERMP))
 (176 176 (:META RP::BINARY-OR**/AND**-GUARD-META-CORRECT))
 (168 16 (:REWRITE RP::NOT-INCLUDE-RP))
 (124 8 (:LINEAR APPLY$-BADGEP-PROPERTIES . 1))
 (119 119 (:TYPE-PRESCRIPTION RP::INCLUDE-FNC-FN))
 (118 8 (:DEFINITION APPLY$-BADGEP))
 (96 10 (:REWRITE RP::RP-TERMP-CADR))
 (96 10 (:REWRITE RP::IS-IF-RP-TERMP))
 (80 4 (:DEFINITION RP::RP-TERM-LISTP))
 (78 78 (:TYPE-PRESCRIPTION RP::INCLUDE-FNC-SUBTERMS-FN))
 (68 2 (:DEFINITION RP::EX-FROM-RP))
 (61 43 (:REWRITE RP::RP-EVL-OF-ZP-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-UNARY-/-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-UNARY---CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-TYPESPEC-CHECK-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-SYNP-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-SYMBOLP-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-SYMBOL-PACKAGE-NAME-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-SYMBOL-NAME-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-STRINGP-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-RP-EQUAL-SUBTERMS-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-RP-EQUAL-CNT-SUBTERMS-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-RP-EQUAL-CNT-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-RP-EQUAL-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-RP-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-RETURN-LAST-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-REALPART-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-RATIONALP-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-NUMERATOR-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-NOT-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-NATP-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-LAMBDA))
 (61 43 (:REWRITE RP::RP-EVL-OF-INTERN-IN-PACKAGE-OF-SYMBOL-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-INTEGERP-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-IMPLIES-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-IMAGPART-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-IFF-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-IF-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-HIDE-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-FORCE-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-FORCE$-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-FALIST-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-EQUALS-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-EQUAL-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-DONT-RW-CONTEXT-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-DENOMINATOR-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-CONSP-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-CONS-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-COMPLEX-RATIONALP-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-COERCE-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-CODE-CHAR-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-CHARACTERP-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-CHAR-CODE-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-CDR-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-CASESPLIT-FROM-CONTEXT-TRIG-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-CAR-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-BITP-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-BINARY-+-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-BINARY-*-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-BADGE-USERFN-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-BAD-ATOM<=-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-APPLY$-USERFN-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-APPLY$-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-ACL2-NUMBERP-CALL))
 (61 43 (:REWRITE RP::RP-EVL-OF-<-CALL))
 (48 4 (:DEFINITION NATP))
 (40 8 (:REWRITE RP::RP-TERMP-IMPLIES-SUBTERMS))
 (39 39 (:REWRITE SYMBOL-LISTP-IMPLIES-SYMBOLP))
 (38 38 (:REWRITE FN-CHECK-DEF-NOT-QUOTE))
 (38 8 (:REWRITE RP::VALID-SC-CADDR))
 (26 26 (:TYPE-PRESCRIPTION APPLY$-BADGEP))
 (24 8 (:DEFINITION WEAK-APPLY$-BADGE-P))
 (16 8 (:REWRITE RP::RP-TERMP-IMPLIES-CDR-LISTP))
 (14 6 (:REWRITE APPLY$-BADGEP-PROPERTIES . 2))
 (14 4 (:LINEAR APPLY$-BADGEP-PROPERTIES . 2))
 (12 6 (:REWRITE APPLY$-BADGEP-PROPERTIES . 1))
 (8 8 (:TYPE-PRESCRIPTION RP::RP-TERM-LISTP))
 (8 8 (:TYPE-PRESCRIPTION QUOTEP))
 (8 2 (:REWRITE RP::VALID-SC-OF-EX-FROM-RP))
 (8 2 (:REWRITE RP::VALID-SC-EX-FROM-RP))
 (6 6 (:TYPE-PRESCRIPTION RP::CONTEXT-FROM-RP))
 (6 6 (:REWRITE INTEGER-LISTP-IMPLIES-INTEGERP))
 (2 2 (:TYPE-PRESCRIPTION RP::EVAL-AND-ALL))
 (2 2 (:REWRITE RP::VALID-SC-CADDDR))
 (2 2 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE DEFAULT-<-1))
 )
(RP::RP-TERMP-FAST-ALIST-FREE-RP-META
 (154 22 (:REWRITE RP::RP-TERMP-IMPLIES-SUBTERMS))
 (135 135 (:REWRITE DEFAULT-CAR))
 (98 98 (:REWRITE DEFAULT-CDR))
 (93 23 (:REWRITE RP::IS-IF-RP-TERMP))
 (88 22 (:REWRITE RP::RP-TERMP-IMPLIES-CDR-LISTP))
 (69 18 (:REWRITE RP::RP-TERMP-CADR))
 (48 24 (:DEFINITION QUOTEP))
 (22 22 (:TYPE-PRESCRIPTION QUOTEP))
 (22 5 (:REWRITE RP::RP-TERMP-CADDR))
 (17 5 (:REWRITE RP::RP-TERMP-SINGLE-STEP-3))
 (12 2 (:REWRITE RP::NOT-INCLUDE-RP))
 (10 4 (:REWRITE RP::ATOM-RP-TERMP-IS-SYMBOLP))
 (9 9 (:META RP::BINARY-OR**/AND**-GUARD-META-CORRECT))
 (8 2 (:DEFINITION RP::INCLUDE-FNC-FN))
 (4 4 (:REWRITE SYMBOL-LISTP-IMPLIES-SYMBOLP))
 (3 3 (:REWRITE FN-CHECK-DEF-NOT-QUOTE))
 (2 2 (:TYPE-PRESCRIPTION RP::INCLUDE-FNC-FN))
 )
(RP::RP-EVL-OF-FAST-ALIST-FREE-META
 (529 409 (:REWRITE DEFAULT-CAR))
 (515 455 (:REWRITE DEFAULT-CDR))
 (192 33 (:REWRITE RP::RP-EVL-OF-VARIABLE))
 (171 171 (:TYPE-PRESCRIPTION RP::TRANS-LIST))
 (127 26 (:REWRITE RP::ATOM-RP-TERMP-IS-SYMBOLP))
 (124 16 (:REWRITE RP::RP-TRANS-IS-TERM-WHEN-LIST-IS-ABSENT))
 (106 106 (:META RP::BINARY-OR**/AND**-GUARD-META-CORRECT))
 (90 9 (:DEFINITION RP::INCLUDE-FNC-FN))
 (90 5 (:DEFINITION RP::RP-TERMP))
 (48 5 (:REWRITE RP::RP-TERMP-CADR))
 (48 5 (:REWRITE RP::IS-IF-RP-TERMP))
 (42 33 (:REWRITE RP::RP-EVL-OF-ZP-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-UNARY-/-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-UNARY---CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-TYPESPEC-CHECK-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-SYNP-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-SYMBOLP-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-SYMBOL-PACKAGE-NAME-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-SYMBOL-NAME-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-STRINGP-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-RP-EQUAL-SUBTERMS-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-RP-EQUAL-CNT-SUBTERMS-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-RP-EQUAL-CNT-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-RP-EQUAL-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-RP-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-RETURN-LAST-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-REALPART-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-RATIONALP-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-QUOTE))
 (42 33 (:REWRITE RP::RP-EVL-OF-NUMERATOR-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-NOT-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-NATP-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-LAMBDA))
 (42 33 (:REWRITE RP::RP-EVL-OF-INTERN-IN-PACKAGE-OF-SYMBOL-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-INTEGERP-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-IMPLIES-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-IMAGPART-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-IFF-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-IF-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-HIDE-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-FORCE-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-FORCE$-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-FALIST-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-EQUALS-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-EQUAL-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-DONT-RW-CONTEXT-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-DENOMINATOR-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-CONSP-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-CONS-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-COMPLEX-RATIONALP-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-COERCE-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-CODE-CHAR-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-CHARACTERP-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-CHAR-CODE-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-CDR-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-CASESPLIT-FROM-CONTEXT-TRIG-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-CAR-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-BITP-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-BINARY-+-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-BINARY-*-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-BADGE-USERFN-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-BAD-ATOM<=-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-APPLY$-USERFN-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-APPLY$-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-ACL2-NUMBERP-CALL))
 (42 33 (:REWRITE RP::RP-EVL-OF-<-CALL))
 (40 2 (:DEFINITION RP::RP-TERM-LISTP))
 (27 27 (:TYPE-PRESCRIPTION RP::INCLUDE-FNC-FN))
 (27 27 (:REWRITE FN-CHECK-DEF-NOT-QUOTE))
 (26 26 (:REWRITE SYMBOL-LISTP-IMPLIES-SYMBOLP))
 (20 4 (:REWRITE RP::RP-TERMP-IMPLIES-SUBTERMS))
 (18 18 (:TYPE-PRESCRIPTION RP::INCLUDE-FNC-SUBTERMS-FN))
 (8 4 (:REWRITE RP::RP-TERMP-IMPLIES-CDR-LISTP))
 (4 4 (:TYPE-PRESCRIPTION RP::RP-TERM-LISTP))
 (4 4 (:TYPE-PRESCRIPTION QUOTEP))
 )
(RP::DONT-RW-SYNTAXP-FAST-ALIST-FREE-META)
(RP::FAST-ALIST-FREE-META_FOR_FAST-ALIST-FREE_VALID
 (526 1 (:DEFINITION RP::VALID-SC))
 (280 7 (:DEFINITION RP::RP-TRANS))
 (170 2 (:DEFINITION RP::RP-TERMP))
 (151 109 (:REWRITE DEFAULT-CAR))
 (136 115 (:REWRITE DEFAULT-CDR))
 (126 7 (:DEFINITION RP::TRANS-LIST))
 (117 1 (:DEFINITION RP::EVAL-AND-ALL))
 (77 9 (:DEFINITION RP::INCLUDE-FNC-FN))
 (67 6 (:REWRITE RP::RP-TRANS-IS-TERM-WHEN-LIST-IS-ABSENT))
 (63 63 (:TYPE-PRESCRIPTION RP::RP-TRANS-LST))
 (56 18 (:DEFINITION QUOTEP))
 (44 44 (:META RP::BINARY-OR**/AND**-GUARD-META-CORRECT))
 (36 4 (:REWRITE RP::NOT-INCLUDE-RP))
 (32 8 (:REWRITE RP::IS-IF-RP-TERMP))
 (28 28 (:REWRITE RP::CONSP-RP-TRANS-LST))
 (23 23 (:TYPE-PRESCRIPTION RP::INCLUDE-FNC-FN))
 (19 7 (:REWRITE RP::RP-EVL-OF-VARIABLE))
 (16 10 (:REWRITE RP::ATOM-RP-TERMP-IS-SYMBOLP))
 (16 4 (:REWRITE RP::RP-TERMP-CADR))
 (16 4 (:REWRITE RP::RP-TERMP-CADDR))
 (16 1 (:DEFINITION RP::EX-FROM-RP))
 (14 14 (:TYPE-PRESCRIPTION RP::INCLUDE-FNC-SUBTERMS-FN))
 (14 4 (:REWRITE RP::RP-TERMP-SINGLE-STEP-3))
 (14 2 (:REWRITE RP::RP-TERMP-IMPLIES-SUBTERMS))
 (13 13 (:TYPE-PRESCRIPTION RP::EX-FROM-SYNP))
 (10 10 (:REWRITE SYMBOL-LISTP-IMPLIES-SYMBOLP))
 (10 10 (:REWRITE FN-CHECK-DEF-NOT-QUOTE))
 (8 2 (:REWRITE RP::RP-TERMP-IMPLIES-CDR-LISTP))
 (7 7 (:REWRITE RP::RP-EVL-OF-ZP-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-UNARY-/-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-UNARY---CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-TYPESPEC-CHECK-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-SYNP-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-SYMBOLP-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-SYMBOL-PACKAGE-NAME-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-SYMBOL-NAME-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-STRINGP-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-RP-EQUAL-SUBTERMS-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-RP-EQUAL-CNT-SUBTERMS-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-RP-EQUAL-CNT-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-RP-EQUAL-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-RP-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-RETURN-LAST-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-REALPART-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-RATIONALP-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-QUOTE))
 (7 7 (:REWRITE RP::RP-EVL-OF-NUMERATOR-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-NOT-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-NATP-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-LAMBDA))
 (7 7 (:REWRITE RP::RP-EVL-OF-INTERN-IN-PACKAGE-OF-SYMBOL-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-INTEGERP-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-IMPLIES-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-IMAGPART-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-IFF-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-IF-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-HIDE-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-FORCE-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-FORCE$-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-FALIST-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-EQUALS-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-EQUAL-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-DONT-RW-CONTEXT-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-DENOMINATOR-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-CONSP-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-CONS-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-COMPLEX-RATIONALP-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-COERCE-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-CODE-CHAR-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-CHARACTERP-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-CHAR-CODE-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-CDR-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-CASESPLIT-FROM-CONTEXT-TRIG-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-CAR-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-BITP-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-BINARY-+-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-BINARY-*-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-BADGE-USERFN-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-BAD-ATOM<=-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-APPLY$-USERFN-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-APPLY$-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-ACL2-NUMBERP-CALL))
 (7 7 (:REWRITE RP::RP-EVL-OF-<-CALL))
 (5 3 (:REWRITE RP::VALID-SC-CADR))
 (4 2 (:REWRITE RP::VALID-SC-CADDR))
 (4 1 (:REWRITE RP::VALID-SC-OF-EX-FROM-RP))
 (4 1 (:REWRITE RP::VALID-SC-EX-FROM-RP))
 (3 3 (:TYPE-PRESCRIPTION RP::CONTEXT-FROM-RP))
 (3 1 (:REWRITE RP::VALID-SC-CADDDR))
 (2 2 (:TYPE-PRESCRIPTION QUOTEP))
 (1 1 (:TYPE-PRESCRIPTION RP::IS-IF$INLINE))
 (1 1 (:TYPE-PRESCRIPTION RP::IS-EQUALS$INLINE))
 (1 1 (:TYPE-PRESCRIPTION RP::EVAL-AND-ALL))
 )
