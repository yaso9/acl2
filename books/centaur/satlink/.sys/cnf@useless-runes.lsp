(SATLINK::EQUAL-1-WHEN-BITP)
(SATLINK::EVAL-VAR$INLINE
 (60 2 (:DEFINITION NTH))
 (50 10 (:DEFINITION LEN))
 (22 12 (:REWRITE DEFAULT-+-2))
 (20 14 (:REWRITE DEFAULT-<-2))
 (16 16 (:TYPE-PRESCRIPTION BITP))
 (15 14 (:REWRITE DEFAULT-<-1))
 (13 13 (:REWRITE DEFAULT-CDR))
 (12 12 (:REWRITE DEFAULT-+-1))
 (8 2 (:REWRITE ZP-WHEN-INTEGERP))
 (8 2 (:REWRITE BFIX-WHEN-NOT-1))
 (6 6 (:LINEAR LEQ-POSITION-EQUAL-LEN))
 (6 2 (:REWRITE ZP-WHEN-GT-0))
 (6 2 (:REWRITE SATLINK::EQUAL-1-WHEN-BITP))
 (6 2 (:REWRITE BFIX-WHEN-NOT-BITP))
 (6 2 (:REWRITE BFIX-WHEN-BITP))
 (6 2 (:REWRITE BFIX-BITP))
 (5 5 (:REWRITE NFIX-WHEN-NOT-NATP))
 (4 4 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (4 2 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (4 2 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (3 3 (:REWRITE DEFAULT-CAR))
 (3 1 (:DEFINITION BITARR$AP))
 (2 2 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 )
(SATLINK::BITP-OF-EVAL-VAR)
(SATLINK::EVAL-VAR$INLINE-OF-VAR-FIX-VAR)
(SATLINK::EVAL-VAR$INLINE-VAR-EQUIV-CONGRUENCE-ON-VAR)
(SATLINK::EVAL-LIT)
(SATLINK::BITP-OF-EVAL-LIT)
(SATLINK::EVAL-LIT-OF-LIT-FIX-LIT)
(SATLINK::EVAL-LIT-LIT-EQUIV-CONGRUENCE-ON-LIT)
(SATLINK::EVAL-CLAUSE
 (120 2 (:DEFINITION SATLINK::EVAL-CLAUSE))
 (84 6 (:REWRITE BFIX-WHEN-NOT-1))
 (12 6 (:REWRITE BFIX-WHEN-NOT-BITP))
 (12 6 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (12 6 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (8 8 (:REWRITE DEFAULT-CAR))
 (6 6 (:REWRITE DEFAULT-CDR))
 (4 4 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (4 4 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (4 4 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (3 3 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (3 1 (:DEFINITION BITARR$AP))
 )
(SATLINK::BITP-OF-EVAL-CLAUSE)
(SATLINK::EVAL-CLAUSE-OF-LIT-LIST-FIX-CLAUSE
 (1236 93 (:REWRITE BFIX-WHEN-NOT-1))
 (196 37 (:REWRITE SATLINK::LIT-LIST-FIX-WHEN-LIT-LISTP))
 (186 186 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (186 93 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (186 93 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (181 93 (:REWRITE BFIX-WHEN-NOT-BITP))
 (176 84 (:REWRITE DEFAULT-CDR))
 (97 97 (:TYPE-PRESCRIPTION SATLINK::LIT-LISTP))
 (66 58 (:REWRITE DEFAULT-CAR))
 (50 49 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (48 12 (:REWRITE SATLINK::LIT-LISTP-OF-CDR-WHEN-LIT-LISTP))
 (33 11 (:REWRITE SATLINK::LIT-FIX-OF-LIT))
 (22 22 (:TYPE-PRESCRIPTION SATLINK::LITP))
 (4 4 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (4 4 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (4 4 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 )
(SATLINK::EVAL-CLAUSE-LIT-LIST-EQUIV-CONGRUENCE-ON-CLAUSE)
(SATLINK::EVAL-FORMULA
 (120 2 (:DEFINITION SATLINK::EVAL-FORMULA))
 (84 6 (:REWRITE BFIX-WHEN-NOT-1))
 (12 6 (:REWRITE BFIX-WHEN-NOT-BITP))
 (12 6 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (12 6 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (8 8 (:REWRITE DEFAULT-CAR))
 (6 6 (:REWRITE DEFAULT-CDR))
 (4 4 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (4 4 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (4 4 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (3 3 (:REWRITE SATLINK::LIT-LIST-LISTP-WHEN-NOT-CONSP))
 (3 1 (:DEFINITION BITARR$AP))
 )
(SATLINK::BITP-OF-EVAL-FORMULA)
(SATLINK::EVAL-FORMULA-OF-LIT-LIST-LIST-FIX-FORMULA
 (1024 89 (:REWRITE BFIX-WHEN-NOT-1))
 (185 38 (:REWRITE SATLINK::LIT-LIST-LIST-FIX-WHEN-LIT-LIST-LISTP))
 (178 178 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (178 89 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (178 89 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (161 89 (:REWRITE BFIX-WHEN-NOT-BITP))
 (85 85 (:TYPE-PRESCRIPTION SATLINK::LIT-LIST-LISTP))
 (51 45 (:REWRITE SATLINK::LIT-LIST-LISTP-WHEN-NOT-CONSP))
 (44 11 (:REWRITE SATLINK::LIT-LIST-LISTP-OF-CDR-WHEN-LIT-LIST-LISTP))
 (25 7 (:REWRITE SATLINK::LIT-LIST-FIX-WHEN-LIT-LISTP))
 (12 12 (:TYPE-PRESCRIPTION SATLINK::LIT-LISTP))
 (6 6 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (4 4 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (4 4 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (4 4 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT SATLINK::LIT-LIST-FIX-UNDER-LIT-LIST-EQUIV))
 )
(SATLINK::EVAL-FORMULA-LIT-LIST-LIST-EQUIV-CONGRUENCE-ON-FORMULA)
(SATLINK::EVAL-CUBE
 (120 2 (:DEFINITION SATLINK::EVAL-CUBE))
 (84 6 (:REWRITE BFIX-WHEN-NOT-1))
 (12 6 (:REWRITE BFIX-WHEN-NOT-BITP))
 (12 6 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (12 6 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (8 8 (:REWRITE DEFAULT-CAR))
 (6 6 (:REWRITE DEFAULT-CDR))
 (4 4 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (4 4 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (4 4 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (3 3 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (3 1 (:DEFINITION BITARR$AP))
 )
(SATLINK::BITP-OF-EVAL-CUBE)
(SATLINK::EVAL-CUBE-OF-LIT-LIST-FIX-CUBE
 (1025 90 (:REWRITE BFIX-WHEN-NOT-1))
 (185 38 (:REWRITE SATLINK::LIT-LIST-FIX-WHEN-LIT-LISTP))
 (180 180 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (180 90 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (180 90 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (162 90 (:REWRITE BFIX-WHEN-NOT-BITP))
 (85 85 (:TYPE-PRESCRIPTION SATLINK::LIT-LISTP))
 (51 45 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (44 11 (:REWRITE SATLINK::LIT-LISTP-OF-CDR-WHEN-LIT-LISTP))
 (19 7 (:REWRITE SATLINK::LIT-FIX-OF-LIT))
 (12 12 (:TYPE-PRESCRIPTION SATLINK::LITP))
 (4 4 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (4 4 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (4 4 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 )
(SATLINK::EVAL-CUBE-LIT-LIST-EQUIV-CONGRUENCE-ON-CUBE)
(SATLINK::FAST-MAX-INDEX-CLAUSE
 (3 3 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (3 2 (:REWRITE DEFAULT-<-1))
 (3 1 (:REWRITE NATP-WHEN-GTE-0))
 (2 2 (:REWRITE NFIX-WHEN-NOT-NATP))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE DEFAULT-<-2))
 (1 1 (:REWRITE NATP-WHEN-INTEGERP))
 (1 1 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (1 1 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (1 1 (:REWRITE DEFAULT-CDR))
 )
(SATLINK::FAST-MAX-INDEX-CLAUSE-OF-LIT-LIST-FIX-CLAUSE
 (415 192 (:REWRITE NFIX-WHEN-NOT-NATP))
 (378 213 (:REWRITE DEFAULT-CDR))
 (306 54 (:REWRITE SATLINK::LIT-LIST-FIX-WHEN-LIT-LISTP))
 (303 178 (:REWRITE DEFAULT-<-1))
 (254 178 (:REWRITE DEFAULT-<-2))
 (180 180 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (180 180 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (176 176 (:REWRITE DEFAULT-CAR))
 (150 72 (:REWRITE NATP-WHEN-GTE-0))
 (147 147 (:TYPE-PRESCRIPTION SATLINK::LIT-LISTP))
 (88 22 (:REWRITE SATLINK::LIT-LISTP-OF-CDR-WHEN-LIT-LISTP))
 (83 76 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (72 72 (:REWRITE NATP-WHEN-INTEGERP))
 (32 4 (:REWRITE NFIX-EQUAL-TO-NONZERO))
 (12 4 (:REWRITE ZP-WHEN-GT-0))
 (8 8 (:TYPE-PRESCRIPTION ZP))
 (4 4 (:REWRITE ZP-WHEN-INTEGERP))
 (4 4 (:REWRITE ZP-OPEN))
 (4 4 (:REWRITE NFIX-EQUAL-TO-NONZERO-CONST))
 )
(SATLINK::FAST-MAX-INDEX-CLAUSE-LIT-LIST-EQUIV-CONGRUENCE-ON-CLAUSE)
(SATLINK::FAST-MAX-INDEX-CLAUSE-OF-NFIX-MAX
 (111 31 (:REWRITE NFIX-WHEN-NOT-NATP))
 (52 22 (:REWRITE DEFAULT-<-2))
 (38 22 (:REWRITE DEFAULT-<-1))
 (25 13 (:REWRITE NATP-WHEN-GTE-0))
 (24 24 (:TYPE-PRESCRIPTION NATP))
 (18 18 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (18 18 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (18 18 (:REWRITE DEFAULT-CDR))
 (18 18 (:REWRITE DEFAULT-CAR))
 (13 13 (:REWRITE NATP-WHEN-INTEGERP))
 )
(SATLINK::FAST-MAX-INDEX-CLAUSE-NAT-EQUIV-CONGRUENCE-ON-MAX)
(SATLINK::MAX-INDEX-CLAUSE)
(SATLINK::NATP-OF-MAX-INDEX-CLAUSE)
(SATLINK::FAST-MAX-INDEX-CLAUSE-REMOVAL
 (182 55 (:REWRITE NFIX-WHEN-NOT-NATP))
 (159 92 (:REWRITE DEFAULT-<-1))
 (149 92 (:REWRITE DEFAULT-<-2))
 (108 54 (:REWRITE NATP-WHEN-GTE-0))
 (55 55 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (55 55 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (55 55 (:REWRITE DEFAULT-CAR))
 (54 54 (:REWRITE NATP-WHEN-INTEGERP))
 (52 52 (:REWRITE DEFAULT-CDR))
 (26 1 (:REWRITE NFIX-EQUAL-TO-NONZERO))
 (15 2 (:REWRITE ZP-WHEN-INTEGERP))
 (11 2 (:REWRITE ZP-WHEN-GT-0))
 (1 1 (:TYPE-PRESCRIPTION ZP))
 (1 1 (:REWRITE ZP-OPEN))
 (1 1 (:REWRITE NFIX-EQUAL-TO-NONZERO-CONST))
 )
(SATLINK::MAX-INDEX-CLAUSE
 (20 7 (:REWRITE DEFAULT-<-1))
 (13 7 (:REWRITE DEFAULT-<-2))
 (7 7 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (7 7 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (7 7 (:REWRITE DEFAULT-CDR))
 (7 7 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 )
(SATLINK::MAX-INDEX-CLAUSE-OF-LIT-LIST-FIX-CLAUSE
 (379 232 (:REWRITE DEFAULT-CDR))
 (268 137 (:REWRITE DEFAULT-<-1))
 (261 48 (:REWRITE SATLINK::LIT-LIST-FIX-WHEN-LIT-LISTP))
 (256 137 (:REWRITE DEFAULT-<-2))
 (162 162 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (162 162 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (161 161 (:REWRITE DEFAULT-CAR))
 (127 127 (:TYPE-PRESCRIPTION SATLINK::LIT-LISTP))
 (69 65 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (68 17 (:REWRITE SATLINK::LIT-LISTP-OF-CDR-WHEN-LIT-LISTP))
 (2 2 (:REWRITE SATLINK::EQUAL-OF-LIT->VAR-NEGATED-HYP))
 (2 2 (:REWRITE SATLINK::EQUAL-OF-LIT->VAR-EQUAL-HYP))
 )
(SATLINK::MAX-INDEX-CLAUSE-LIT-LIST-EQUIV-CONGRUENCE-ON-CLAUSE)
(SATLINK::FAST-MAX-INDEX-FORMULA
 (3 3 (:REWRITE SATLINK::LIT-LIST-LISTP-WHEN-NOT-CONSP))
 (3 2 (:REWRITE DEFAULT-<-1))
 (3 1 (:REWRITE NATP-WHEN-GTE-0))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE DEFAULT-<-2))
 (1 1 (:REWRITE NFIX-WHEN-NOT-NATP))
 (1 1 (:REWRITE NATP-WHEN-INTEGERP))
 (1 1 (:REWRITE DEFAULT-CDR))
 )
(SATLINK::FAST-MAX-INDEX-FORMULA-OF-LIT-LIST-LIST-FIX-FORMULA
 (415 192 (:REWRITE NFIX-WHEN-NOT-NATP))
 (378 213 (:REWRITE DEFAULT-CDR))
 (306 54 (:REWRITE SATLINK::LIT-LIST-LIST-FIX-WHEN-LIT-LIST-LISTP))
 (303 178 (:REWRITE DEFAULT-<-1))
 (254 178 (:REWRITE DEFAULT-<-2))
 (176 176 (:REWRITE DEFAULT-CAR))
 (150 72 (:REWRITE NATP-WHEN-GTE-0))
 (147 147 (:TYPE-PRESCRIPTION SATLINK::LIT-LIST-LISTP))
 (88 22 (:REWRITE SATLINK::LIT-LIST-LISTP-OF-CDR-WHEN-LIT-LIST-LISTP))
 (83 76 (:REWRITE SATLINK::LIT-LIST-LISTP-WHEN-NOT-CONSP))
 (72 72 (:REWRITE NATP-WHEN-INTEGERP))
 (32 4 (:REWRITE NFIX-EQUAL-TO-NONZERO))
 (12 4 (:REWRITE ZP-WHEN-GT-0))
 (8 8 (:TYPE-PRESCRIPTION ZP))
 (4 4 (:REWRITE ZP-WHEN-INTEGERP))
 (4 4 (:REWRITE ZP-OPEN))
 (4 4 (:REWRITE NFIX-EQUAL-TO-NONZERO-CONST))
 )
(SATLINK::FAST-MAX-INDEX-FORMULA-LIT-LIST-LIST-EQUIV-CONGRUENCE-ON-FORMULA)
(SATLINK::FAST-MAX-INDEX-FORMULA-OF-NFIX-MAX
 (111 31 (:REWRITE NFIX-WHEN-NOT-NATP))
 (52 22 (:REWRITE DEFAULT-<-2))
 (38 22 (:REWRITE DEFAULT-<-1))
 (25 13 (:REWRITE NATP-WHEN-GTE-0))
 (24 24 (:TYPE-PRESCRIPTION NATP))
 (18 18 (:REWRITE DEFAULT-CDR))
 (18 18 (:REWRITE DEFAULT-CAR))
 (13 13 (:REWRITE NATP-WHEN-INTEGERP))
 )
(SATLINK::FAST-MAX-INDEX-FORMULA-NAT-EQUIV-CONGRUENCE-ON-MAX)
(SATLINK::MAX-INDEX-FORMULA)
(SATLINK::NATP-OF-MAX-INDEX-FORMULA)
(SATLINK::FAST-MAX-INDEX-FORMULA-REMOVAL
 (182 55 (:REWRITE NFIX-WHEN-NOT-NATP))
 (159 92 (:REWRITE DEFAULT-<-1))
 (149 92 (:REWRITE DEFAULT-<-2))
 (108 54 (:REWRITE NATP-WHEN-GTE-0))
 (55 55 (:REWRITE DEFAULT-CAR))
 (54 54 (:REWRITE NATP-WHEN-INTEGERP))
 (52 52 (:REWRITE DEFAULT-CDR))
 (26 1 (:REWRITE NFIX-EQUAL-TO-NONZERO))
 (15 2 (:REWRITE ZP-WHEN-INTEGERP))
 (11 2 (:REWRITE ZP-WHEN-GT-0))
 (1 1 (:TYPE-PRESCRIPTION ZP))
 (1 1 (:REWRITE ZP-OPEN))
 (1 1 (:REWRITE NFIX-EQUAL-TO-NONZERO-CONST))
 )
(SATLINK::MAX-INDEX-FORMULA
 (20 7 (:REWRITE DEFAULT-<-1))
 (13 7 (:REWRITE DEFAULT-<-2))
 (7 7 (:REWRITE DEFAULT-CDR))
 (7 7 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE SATLINK::LIT-LIST-LISTP-WHEN-NOT-CONSP))
 )
(SATLINK::MAX-INDEX-FORMULA-OF-LIT-LIST-LIST-FIX-FORMULA
 (379 232 (:REWRITE DEFAULT-CDR))
 (268 137 (:REWRITE DEFAULT-<-1))
 (261 48 (:REWRITE SATLINK::LIT-LIST-LIST-FIX-WHEN-LIT-LIST-LISTP))
 (256 137 (:REWRITE DEFAULT-<-2))
 (161 161 (:REWRITE DEFAULT-CAR))
 (127 127 (:TYPE-PRESCRIPTION SATLINK::LIT-LIST-LISTP))
 (69 65 (:REWRITE SATLINK::LIT-LIST-LISTP-WHEN-NOT-CONSP))
 (68 17 (:REWRITE SATLINK::LIT-LIST-LISTP-OF-CDR-WHEN-LIT-LIST-LISTP))
 )
(SATLINK::MAX-INDEX-FORMULA-LIT-LIST-LIST-EQUIV-CONGRUENCE-ON-FORMULA)
(SATLINK::CLAUSE-INDICES
 (3 3 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (1 1 (:REWRITE DEFAULT-CDR))
 (1 1 (:REWRITE DEFAULT-CAR))
 )
(SATLINK::CLAUSE-INDICES-WHEN-ATOM)
(SATLINK::CLAUSE-INDICES-OF-CONS
 (6 6 (:REWRITE SATLINK::CLAUSE-INDICES-WHEN-ATOM))
 (5 5 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (5 5 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (3 3 (:REWRITE DEFAULT-CDR))
 (3 3 (:REWRITE DEFAULT-CAR))
 )
(SATLINK::CLAUSE-INDICES-OF-LIT-LIST-FIX-CLAUSE
 (85 44 (:REWRITE DEFAULT-CDR))
 (76 13 (:REWRITE SATLINK::LIT-LIST-FIX-WHEN-LIT-LISTP))
 (37 37 (:TYPE-PRESCRIPTION SATLINK::LIT-LISTP))
 (32 30 (:REWRITE DEFAULT-CAR))
 (28 28 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (28 28 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (24 6 (:REWRITE SATLINK::LIT-LISTP-OF-CDR-WHEN-LIT-LISTP))
 (20 19 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 )
(SATLINK::CLAUSE-INDICES-LIT-LIST-EQUIV-CONGRUENCE-ON-CLAUSE)
(SATLINK::FORMULA-INDICES
 (4 4 (:TYPE-PRESCRIPTION CONSP-APPEND . 1))
 (3 3 (:REWRITE SATLINK::LIT-LIST-LISTP-WHEN-NOT-CONSP))
 (2 2 (:REWRITE DEFAULT-CAR))
 (1 1 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (1 1 (:REWRITE DEFAULT-CDR))
 (1 1 (:REWRITE SATLINK::CLAUSE-INDICES-WHEN-ATOM))
 )
(SATLINK::FORMULA-INDICES-WHEN-ATOM)
(SATLINK::FORMULA-INDICES-OF-CONS
 (25 5 (:DEFINITION BINARY-APPEND))
 (13 11 (:TYPE-PRESCRIPTION CONSP-APPEND . 1))
 (8 8 (:REWRITE DEFAULT-CDR))
 (8 8 (:REWRITE DEFAULT-CAR))
 (6 6 (:REWRITE SATLINK::FORMULA-INDICES-WHEN-ATOM))
 (5 5 (:REWRITE SATLINK::CLAUSE-INDICES-WHEN-ATOM))
 )
(SATLINK::FORMULA-INDICES-OF-LIT-LIST-LIST-FIX-FORMULA
 (190 38 (:DEFINITION BINARY-APPEND))
 (154 127 (:TYPE-PRESCRIPTION CONSP-APPEND . 1))
 (129 84 (:REWRITE DEFAULT-CDR))
 (84 15 (:REWRITE SATLINK::LIT-LIST-LIST-FIX-WHEN-LIT-LIST-LISTP))
 (74 74 (:REWRITE DEFAULT-CAR))
 (41 41 (:TYPE-PRESCRIPTION SATLINK::LIT-LIST-LISTP))
 (38 38 (:REWRITE SATLINK::CLAUSE-INDICES-WHEN-ATOM))
 (24 6 (:REWRITE SATLINK::LIT-LIST-LISTP-OF-CDR-WHEN-LIT-LIST-LISTP))
 (22 21 (:REWRITE SATLINK::LIT-LIST-LISTP-WHEN-NOT-CONSP))
 )
(SATLINK::FORMULA-INDICES-LIT-LIST-LIST-EQUIV-CONGRUENCE-ON-FORMULA)
(SATLINK::LIT-ASSUMEDP
 (10 2 (:DEFINITION LEN))
 (8 4 (:REWRITE DEFAULT-+-2))
 (7 5 (:REWRITE DEFAULT-<-1))
 (6 5 (:REWRITE DEFAULT-<-2))
 (4 4 (:REWRITE DEFAULT-+-1))
 (4 2 (:REWRITE DEFAULT-*-2))
 (3 3 (:REWRITE DEFAULT-CDR))
 (3 1 (:DEFINITION BITARR$AP))
 (2 2 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (2 2 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (2 2 (:REWRITE DEFAULT-*-1))
 (1 1 (:REWRITE DEFAULT-CAR))
 (1 1 (:LINEAR LEQ-POSITION-EQUAL-LEN))
 )
(SATLINK::LIT-ASSUMEDP-OF-LIT-FIX-LIT)
(SATLINK::LIT-ASSUMEDP-LIT-EQUIV-CONGRUENCE-ON-LIT)
(SATLINK::LIT-ASSUME
 (10 2 (:DEFINITION LEN))
 (8 4 (:REWRITE DEFAULT-+-2))
 (7 5 (:REWRITE DEFAULT-<-1))
 (6 5 (:REWRITE DEFAULT-<-2))
 (4 4 (:REWRITE DEFAULT-+-1))
 (4 2 (:REWRITE DEFAULT-*-2))
 (3 3 (:REWRITE DEFAULT-CDR))
 (3 1 (:DEFINITION BITARR$AP))
 (2 2 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (2 2 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (2 2 (:REWRITE DEFAULT-*-1))
 (1 1 (:REWRITE DEFAULT-CAR))
 (1 1 (:LINEAR LEQ-POSITION-EQUAL-LEN))
 )
(SATLINK::LIT-ASSUMEDP-OF-LIT-ASSUME
 (154 5 (:REWRITE NTH-WITH-LARGE-INDEX))
 (84 2 (:DEFINITION NTH))
 (32 1 (:REWRITE LEN-UPDATE-NTH1))
 (30 12 (:REWRITE DEFAULT-<-2))
 (30 6 (:DEFINITION LEN))
 (25 1 (:DEFINITION UPDATE-NTH))
 (24 3 (:REWRITE ZP-WHEN-INTEGERP))
 (24 3 (:REWRITE ZP-WHEN-GT-0))
 (23 2 (:REWRITE SATLINK::EQUAL-OF-LIT-FIX-BACKCHAIN))
 (17 10 (:REWRITE DEFAULT-+-2))
 (16 12 (:REWRITE DEFAULT-<-1))
 (16 9 (:REWRITE NFIX-WHEN-NOT-NATP))
 (13 5 (:REWRITE SATLINK::LIT-FIX-OF-LIT))
 (12 2 (:REWRITE SATLINK::EQUAL-1-WHEN-BITP))
 (11 11 (:TYPE-PRESCRIPTION SATLINK::VARP-OF-LIT->VAR))
 (10 10 (:REWRITE DEFAULT-CDR))
 (10 10 (:REWRITE DEFAULT-+-1))
 (9 3 (:DEFINITION NOT))
 (8 8 (:TYPE-PRESCRIPTION SATLINK::LITP))
 (4 4 (:TYPE-PRESCRIPTION BITP))
 (4 4 (:LINEAR LEQ-POSITION-EQUAL-LEN))
 (3 3 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (3 3 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (3 3 (:REWRITE DEFAULT-CAR))
 (3 1 (:REWRITE SATLINK::LIT->VAR$INLINE-OF-LIT-FIX-LIT))
 (1 1 (:REWRITE SATLINK::EQUAL-OF-LIT->VAR-NEGATED-HYP))
 (1 1 (:REWRITE SATLINK::EQUAL-OF-LIT->VAR-EQUAL-HYP))
 )
(SATLINK::LEN-OF-LIT-ASSUME
 (25 1 (:DEFINITION UPDATE-NTH))
 (15 3 (:DEFINITION LEN))
 (12 4 (:REWRITE DEFAULT-<-1))
 (9 5 (:REWRITE DEFAULT-+-2))
 (8 4 (:REWRITE DEFAULT-<-2))
 (8 1 (:REWRITE ZP-WHEN-INTEGERP))
 (8 1 (:REWRITE ZP-WHEN-GT-0))
 (5 5 (:REWRITE DEFAULT-CDR))
 (5 5 (:REWRITE DEFAULT-+-1))
 (4 2 (:REWRITE NFIX-WHEN-NOT-NATP))
 (3 1 (:REWRITE SATLINK::LIT-FIX-OF-LIT))
 (2 2 (:TYPE-PRESCRIPTION SATLINK::LITP))
 (2 2 (:LINEAR LEQ-POSITION-EQUAL-LEN))
 (1 1 (:REWRITE DEFAULT-CAR))
 )
(SATLINK::LIT-ASSUME-OF-LIT-FIX-LIT
 (50 2 (:DEFINITION UPDATE-NTH))
 (16 2 (:REWRITE ZP-WHEN-INTEGERP))
 (16 2 (:REWRITE ZP-WHEN-GT-0))
 (8 4 (:REWRITE DEFAULT-<-2))
 (6 2 (:DEFINITION NOT))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 4 (:REWRITE DEFAULT-<-1))
 (2 2 (:TYPE-PRESCRIPTION SATLINK::LITP))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:REWRITE DEFAULT-+-1))
 )
(SATLINK::LIT-ASSUME-LIT-EQUIV-CONGRUENCE-ON-LIT)
(SATLINK::ENV-SATISFIES-ASSUMS)
(SATLINK::ENV-SATISFIES-ASSUMS-NECC)
(SATLINK::ENV-SATISFIES-ASSUMS-IMPLIES-LIT-EVAL
 (63 3 (:REWRITE BFIX-WHEN-NOT-1))
 (33 8 (:REWRITE SATLINK::ENV-SATISFIES-ASSUMS-NECC))
 (16 16 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (16 16 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (9 9 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (9 9 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (9 9 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (7 3 (:REWRITE BFIX-WHEN-NOT-BITP))
 (6 6 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (6 3 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (6 3 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (3 1 (:REWRITE SATLINK::LIT-FIX-OF-LIT))
 (2 2 (:TYPE-PRESCRIPTION SATLINK::LITP))
 )
(SATLINK::ENV-SATISFIES-ASSUMS-IMPLIES-NOT-LIT-EVAL
 (64 4 (:REWRITE BFIX-WHEN-NOT-1))
 (27 6 (:REWRITE SATLINK::ENV-SATISFIES-ASSUMS-NECC))
 (17 17 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (17 17 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (10 10 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (10 10 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (10 10 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (10 2 (:LINEAR SATLINK::LIT->NEG-BOUND))
 (8 8 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (8 4 (:REWRITE BFIX-WHEN-NOT-BITP))
 (8 4 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (8 4 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (6 2 (:REWRITE SATLINK::LIT-FIX-OF-LIT))
 (4 4 (:TYPE-PRESCRIPTION SATLINK::LITP))
 )
(SATLINK::LIT-ASSUMEDP-OF-OPPOSITE-WHEN-ENV-SATISFIES)
(SATLINK::EQUAL-VAR-FIX-FORWARD
 (9 3 (:REWRITE NFIX-WHEN-NATP))
 (7 3 (:REWRITE NFIX-WHEN-NOT-NATP))
 (4 4 (:TYPE-PRESCRIPTION NATP))
 (4 2 (:REWRITE NATP-WHEN-GTE-0))
 (2 2 (:REWRITE NATP-WHEN-INTEGERP))
 (1 1 (:REWRITE DEFAULT-<-2))
 (1 1 (:REWRITE DEFAULT-<-1))
 )
(SATLINK::EQUAL-VAR-FIX-FORWARD2)
(SATLINK::ENV-SATISFIES-ASSUMS-OF-LIT-ASSUME
 (745 18 (:REWRITE SATLINK::EQUAL-OF-LIT-FIX-BACKCHAIN))
 (312 37 (:REWRITE NFIX-EQUAL-TO-NONZERO))
 (236 5 (:REWRITE SATLINK::ENV-SATISFIES-ASSUMS-IMPLIES-NOT-LIT-EVAL))
 (154 11 (:REWRITE ZP-WHEN-INTEGERP))
 (151 151 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (151 151 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (141 17 (:REWRITE SATLINK::LIT->VAR-OF-MAKE-LIT))
 (133 5 (:REWRITE BFIX-WHEN-NOT-1))
 (107 35 (:REWRITE NFIX-WHEN-NATP))
 (102 2 (:DEFINITION B-NOT$INLINE))
 (102 1 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-BACKCHAIN))
 (88 11 (:REWRITE ZP-WHEN-GT-0))
 (83 35 (:REWRITE NFIX-WHEN-NOT-NATP))
 (78 45 (:REWRITE DEFAULT-<-2))
 (54 1 (:REWRITE SATLINK::LIT->NEG-OF-LIT-NEGATE))
 (48 48 (:TYPE-PRESCRIPTION NATP))
 (48 24 (:REWRITE NATP-WHEN-GTE-0))
 (45 45 (:REWRITE DEFAULT-<-1))
 (44 44 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (44 44 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (44 44 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (37 37 (:REWRITE NFIX-EQUAL-TO-NONZERO-CONST))
 (24 24 (:REWRITE NATP-WHEN-INTEGERP))
 (20 20 (:TYPE-PRESCRIPTION SATLINK::LITP))
 (13 5 (:REWRITE BFIX-WHEN-NOT-BITP))
 (11 11 (:TYPE-PRESCRIPTION ZP))
 (10 10 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (10 5 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (10 5 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (8 8 (:TYPE-PRESCRIPTION SATLINK::LIT-NEGATE$INLINE))
 (3 1 (:REWRITE SATLINK::LIT->VAR-OF-LIT-NEGATE))
 (1 1 (:TYPE-PRESCRIPTION BITP-OF-B-NOT))
 )
(SATLINK::ENV-SATISFIES-ASSUMS-OF-EMPTY-ASSUMS
 (135 2 (:REWRITE SATLINK::LIT-ASSUMEDP-OF-OPPOSITE-WHEN-ENV-SATISFIES))
 (66 4 (:REWRITE NTH-OF-RESIZE-LIST-NEWLY-IN-BOUNDS))
 (42 4 (:REWRITE NTH-OF-RESIZE-LIST-TOO-BIG))
 (36 20 (:REWRITE NFIX-WHEN-NOT-NATP))
 (30 16 (:REWRITE DEFAULT-<-1))
 (28 4 (:REWRITE NTH-OF-RESIZE-LIST-PRESERVED))
 (24 16 (:REWRITE DEFAULT-<-2))
 (8 8 (:REWRITE INEQUALITY-WITH-NFIX-HYP-1))
 (5 1 (:DEFINITION RESIZE-LIST))
 (4 4 (:TYPE-PRESCRIPTION NATP))
 (4 4 (:TYPE-PRESCRIPTION LEN))
 (4 2 (:REWRITE NATP-WHEN-GTE-0))
 (2 2 (:TYPE-PRESCRIPTION SATLINK::LIT-ASSUMEDP))
 (2 2 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (2 2 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (2 2 (:REWRITE NATP-WHEN-INTEGERP))
 (2 2 (:REWRITE SATLINK::LIT-NEGATE-OF-MAKE-LIT))
 (1 1 (:REWRITE DEFAULT-+-2))
 (1 1 (:REWRITE DEFAULT-+-1))
 )
(SATLINK::CLAUSE-UNSAT-UNDER-ASSUMS
 (25 10 (:REWRITE DEFAULT-+-2))
 (25 5 (:DEFINITION LEN))
 (19 7 (:REWRITE DEFAULT-<-1))
 (15 5 (:REWRITE DEFAULT-*-2))
 (14 6 (:REWRITE SATLINK::LIT-ASSUMEDP-OF-OPPOSITE-WHEN-ENV-SATISFIES))
 (12 12 (:REWRITE DEFAULT-CDR))
 (10 10 (:REWRITE DEFAULT-+-1))
 (9 7 (:REWRITE DEFAULT-<-2))
 (6 6 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (6 2 (:DEFINITION BITARR$AP))
 (5 5 (:REWRITE DEFAULT-CAR))
 (5 5 (:REWRITE DEFAULT-*-1))
 (4 2 (:REWRITE SATLINK::LIT-NEGATE-OF-LIT-NEGATE))
 (3 3 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (3 3 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (2 2 (:REWRITE SATLINK::LIT-EQUIV-OF-LIT-FIX))
 (1 1 (:LINEAR LEQ-POSITION-EQUAL-LEN))
 )
(SATLINK::CLAUSE-UNSAT-UNDER-ASSUMS-NOT-TRUE-WHEN-SATISFIED
 (182 13 (:REWRITE BFIX-WHEN-NOT-1))
 (50 10 (:REWRITE SATLINK::LIT-ASSUMEDP-OF-OPPOSITE-WHEN-ENV-SATISFIES))
 (26 26 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (26 13 (:REWRITE BFIX-WHEN-NOT-BITP))
 (26 13 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (26 13 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (20 10 (:REWRITE SATLINK::LIT-NEGATE-OF-LIT-NEGATE))
 (18 18 (:REWRITE DEFAULT-CAR))
 (18 6 (:REWRITE SATLINK::ENV-SATISFIES-ASSUMS-IMPLIES-LIT-EVAL))
 (10 10 (:REWRITE SATLINK::LIT-EQUIV-OF-LIT-FIX))
 (8 8 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (2 2 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (2 2 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 )
(SATLINK::CLAUSE-UNSAT-UNDER-ASSUMS-OF-LIT-LIST-FIX-CLAUSE
 (350 70 (:REWRITE SATLINK::LIT-ASSUMEDP-OF-OPPOSITE-WHEN-ENV-SATISFIES))
 (178 91 (:REWRITE DEFAULT-CDR))
 (145 28 (:REWRITE SATLINK::LIT-LIST-FIX-WHEN-LIT-LISTP))
 (140 70 (:REWRITE SATLINK::LIT-NEGATE-OF-LIT-NEGATE))
 (135 69 (:REWRITE DEFAULT-CAR))
 (90 90 (:REWRITE SATLINK::CLAUSE-UNSAT-UNDER-ASSUMS-NOT-TRUE-WHEN-SATISFIED))
 (70 70 (:REWRITE SATLINK::LIT-EQUIV-OF-LIT-FIX))
 (69 69 (:TYPE-PRESCRIPTION SATLINK::LIT-LISTP))
 (40 36 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (32 8 (:REWRITE SATLINK::LIT-LISTP-OF-CDR-WHEN-LIT-LISTP))
 )
(SATLINK::CLAUSE-UNSAT-UNDER-ASSUMS-LIT-LIST-EQUIV-CONGRUENCE-ON-CLAUSE)
(SATLINK::CLAUSE-UNIT-UNDER-ASSUMS
 (35 7 (:DEFINITION LEN))
 (33 14 (:REWRITE DEFAULT-+-2))
 (23 9 (:REWRITE DEFAULT-<-1))
 (20 20 (:REWRITE DEFAULT-CDR))
 (19 7 (:REWRITE DEFAULT-*-2))
 (14 14 (:REWRITE DEFAULT-+-1))
 (11 9 (:REWRITE DEFAULT-<-2))
 (10 10 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (10 2 (:REWRITE SATLINK::LIT-ASSUMEDP-OF-OPPOSITE-WHEN-ENV-SATISFIES))
 (7 7 (:REWRITE DEFAULT-*-1))
 (6 2 (:DEFINITION BITARR$AP))
 (5 5 (:REWRITE DEFAULT-CAR))
 (4 2 (:REWRITE SATLINK::LIT-NEGATE-OF-LIT-NEGATE))
 (3 3 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (3 3 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (2 2 (:REWRITE SATLINK::LIT-EQUIV-OF-LIT-FIX))
 (2 2 (:REWRITE SATLINK::CLAUSE-UNSAT-UNDER-ASSUMS-NOT-TRUE-WHEN-SATISFIED))
 (1 1 (:LINEAR LEQ-POSITION-EQUAL-LEN))
 )
(SATLINK::NATP-OF-CLAUSE-UNIT-UNDER-ASSUMS.FINAL-POSSIBLES
 (60 12 (:REWRITE SATLINK::LIT-ASSUMEDP-OF-OPPOSITE-WHEN-ENV-SATISFIES))
 (33 11 (:REWRITE NATP-WHEN-GTE-0))
 (24 12 (:REWRITE SATLINK::LIT-NEGATE-OF-LIT-NEGATE))
 (12 12 (:REWRITE SATLINK::LIT-EQUIV-OF-LIT-FIX))
 (12 12 (:REWRITE DEFAULT-CAR))
 (11 11 (:REWRITE NATP-WHEN-INTEGERP))
 (11 11 (:REWRITE DEFAULT-<-2))
 (11 11 (:REWRITE DEFAULT-<-1))
 (9 9 (:REWRITE DEFAULT-CDR))
 (8 8 (:REWRITE SATLINK::CLAUSE-UNSAT-UNDER-ASSUMS-NOT-TRUE-WHEN-SATISFIED))
 (7 1 (:REWRITE SATLINK::LIT-FIX-OF-LIT))
 (4 1 (:REWRITE SATLINK::LITP-OF-CAR-WHEN-LIT-LISTP))
 (2 2 (:TYPE-PRESCRIPTION SATLINK::LITP))
 (2 2 (:TYPE-PRESCRIPTION SATLINK::LIT-LISTP))
 (1 1 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 )
(SATLINK::RETURN-TYPE-OF-CLAUSE-UNIT-UNDER-ASSUMS.UNIT-LIT
 (120 20 (:REWRITE SATLINK::EQUAL-1-WHEN-BITP))
 (105 21 (:REWRITE SATLINK::LIT-ASSUMEDP-OF-OPPOSITE-WHEN-ENV-SATISFIES))
 (42 21 (:REWRITE SATLINK::LIT-NEGATE-OF-LIT-NEGATE))
 (40 40 (:TYPE-PRESCRIPTION BITP))
 (21 21 (:REWRITE SATLINK::LIT-EQUIV-OF-LIT-FIX))
 (21 21 (:REWRITE DEFAULT-CAR))
 (14 2 (:REWRITE SATLINK::LIT-FIX-OF-LIT))
 (13 13 (:REWRITE DEFAULT-CDR))
 (11 11 (:REWRITE SATLINK::CLAUSE-UNSAT-UNDER-ASSUMS-NOT-TRUE-WHEN-SATISFIED))
 (8 2 (:REWRITE SATLINK::LITP-OF-CAR-WHEN-LIT-LISTP))
 (4 4 (:TYPE-PRESCRIPTION SATLINK::LIT-LISTP))
 (2 2 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 )
(SATLINK::CLAUSE-UNIT-UNDER-ASSUMS-0-IMPLIES-UNSAT
 (230 19 (:REWRITE BFIX-WHEN-NOT-1))
 (65 13 (:REWRITE SATLINK::LIT-ASSUMEDP-OF-OPPOSITE-WHEN-ENV-SATISFIES))
 (38 38 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (38 19 (:REWRITE BFIX-WHEN-NOT-BITP))
 (38 19 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (38 19 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (35 5 (:REWRITE SATLINK::LIT-FIX-OF-LIT))
 (27 9 (:REWRITE SATLINK::ENV-SATISFIES-ASSUMS-IMPLIES-LIT-EVAL))
 (26 13 (:REWRITE SATLINK::LIT-NEGATE-OF-LIT-NEGATE))
 (24 24 (:REWRITE DEFAULT-CAR))
 (22 22 (:REWRITE DEFAULT-CDR))
 (20 5 (:REWRITE SATLINK::LITP-OF-CAR-WHEN-LIT-LISTP))
 (13 13 (:REWRITE SATLINK::LIT-EQUIV-OF-LIT-FIX))
 (10 10 (:TYPE-PRESCRIPTION SATLINK::LITP))
 (10 10 (:TYPE-PRESCRIPTION SATLINK::LIT-LISTP))
 (5 5 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (2 2 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (2 2 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (2 2 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 )
(SATLINK::CLAUSE-UNIT-UNDER-ASSUMS-1-IMPLIES-UNIT
 (292 26 (:REWRITE BFIX-WHEN-NOT-1))
 (100 20 (:REWRITE SATLINK::LIT-ASSUMEDP-OF-OPPOSITE-WHEN-ENV-SATISFIES))
 (63 9 (:REWRITE SATLINK::LIT-FIX-OF-LIT))
 (61 20 (:REWRITE SATLINK::ENV-SATISFIES-ASSUMS-IMPLIES-LIT-EVAL))
 (52 52 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (52 26 (:REWRITE BFIX-WHEN-NOT-BITP))
 (52 26 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (52 26 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (40 20 (:REWRITE SATLINK::LIT-NEGATE-OF-LIT-NEGATE))
 (36 9 (:REWRITE SATLINK::LITP-OF-CAR-WHEN-LIT-LISTP))
 (35 35 (:REWRITE DEFAULT-CAR))
 (32 32 (:REWRITE DEFAULT-CDR))
 (20 20 (:REWRITE SATLINK::LIT-EQUIV-OF-LIT-FIX))
 (18 18 (:TYPE-PRESCRIPTION SATLINK::LITP))
 (18 18 (:TYPE-PRESCRIPTION SATLINK::LIT-LISTP))
 (12 12 (:TYPE-PRESCRIPTION BITP))
 (9 9 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (3 3 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (3 3 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (3 3 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (2 1 (:REWRITE SATLINK::LIT-ASSUMEDP-OF-LIT-FIX-LIT))
 (1 1 (:REWRITE SATLINK::LIT-NEGATE$INLINE-OF-LIT-FIX-LIT))
 )
(SATLINK::CLAUSE-UNIT-UNDER-ASSUMS-UNIT-LIT-IMPLIES-IN-BOUNDS
 (75 15 (:REWRITE SATLINK::LIT-ASSUMEDP-OF-OPPOSITE-WHEN-ENV-SATISFIES))
 (49 7 (:REWRITE SATLINK::LIT-FIX-OF-LIT))
 (34 11 (:REWRITE DEFAULT-<-1))
 (30 15 (:REWRITE SATLINK::LIT-NEGATE-OF-LIT-NEGATE))
 (30 5 (:REWRITE SATLINK::EQUAL-1-WHEN-BITP))
 (28 7 (:REWRITE SATLINK::LITP-OF-CAR-WHEN-LIT-LISTP))
 (22 11 (:REWRITE DEFAULT-<-2))
 (20 20 (:REWRITE DEFAULT-CAR))
 (18 18 (:REWRITE DEFAULT-CDR))
 (15 15 (:REWRITE SATLINK::LIT-EQUIV-OF-LIT-FIX))
 (14 14 (:TYPE-PRESCRIPTION SATLINK::LITP))
 (14 14 (:TYPE-PRESCRIPTION SATLINK::LIT-LISTP))
 (12 12 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (12 12 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (10 10 (:TYPE-PRESCRIPTION BITP))
 (10 10 (:REWRITE SATLINK::CLAUSE-UNSAT-UNDER-ASSUMS-NOT-TRUE-WHEN-SATISFIED))
 (7 7 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 )
(SATLINK::CLAUSE-UNIT-UNDER-ASSUMS-OF-LIT-LIST-FIX-CLAUSE
 (480 96 (:REWRITE SATLINK::LIT-ASSUMEDP-OF-OPPOSITE-WHEN-ENV-SATISFIES))
 (443 265 (:REWRITE DEFAULT-CDR))
 (329 169 (:REWRITE DEFAULT-CAR))
 (267 48 (:REWRITE SATLINK::LIT-LISTP-OF-CDR-WHEN-LIT-LISTP))
 (261 55 (:REWRITE SATLINK::LIT-LIST-FIX-WHEN-LIT-LISTP))
 (192 96 (:REWRITE SATLINK::LIT-NEGATE-OF-LIT-NEGATE))
 (129 125 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 (96 96 (:REWRITE SATLINK::LIT-EQUIV-OF-LIT-FIX))
 (85 4 (:REWRITE SATLINK::EQUAL-OF-LIT-FIX-BACKCHAIN))
 (77 77 (:REWRITE SATLINK::CLAUSE-UNSAT-UNDER-ASSUMS-NOT-TRUE-WHEN-SATISFIED))
 (33 33 (:TYPE-PRESCRIPTION SATLINK::VARP-OF-LIT->VAR))
 (6 6 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (6 6 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 (3 3 (:REWRITE SATLINK::EQUAL-OF-LIT->VAR-NEGATED-HYP))
 (3 3 (:REWRITE SATLINK::EQUAL-OF-LIT->VAR-EQUAL-HYP))
 )
(SATLINK::CLAUSE-UNIT-UNDER-ASSUMS-LIT-LIST-EQUIV-CONGRUENCE-ON-CLAUSE)
(SATLINK::TRIVIAL-UNSAT-P1
 (73 13 (:DEFINITION LEN))
 (50 12 (:REWRITE SATLINK::EQUAL-1-WHEN-BITP))
 (49 22 (:REWRITE DEFAULT-+-2))
 (28 24 (:REWRITE DEFAULT-CDR))
 (25 10 (:REWRITE DEFAULT-<-1))
 (23 9 (:REWRITE DEFAULT-*-2))
 (22 22 (:REWRITE DEFAULT-+-1))
 (20 20 (:TYPE-PRESCRIPTION BITP))
 (14 10 (:REWRITE DEFAULT-<-2))
 (10 10 (:REWRITE SATLINK::LIT-LIST-LISTP-WHEN-NOT-CONSP))
 (9 9 (:REWRITE DEFAULT-CAR))
 (9 9 (:REWRITE DEFAULT-*-1))
 (6 6 (:REWRITE SATLINK::CLAUSE-UNIT-UNDER-ASSUMS-0-IMPLIES-UNSAT))
 (6 2 (:DEFINITION BITARR$AP))
 (5 5 (:LINEAR LEQ-POSITION-EQUAL-LEN))
 (4 4 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COND-COMPONENT-REWRITES))
 (4 4 (:REWRITE SATLINK::EQUAL-OF-LIT-NEGATE-COMPONENT-REWRITES))
 )
(SATLINK::TRIVIAL-UNSAT-P1-CORRECT
 (308 22 (:REWRITE BFIX-WHEN-NOT-1))
 (44 44 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (44 22 (:REWRITE BFIX-WHEN-NOT-BITP))
 (44 22 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (44 22 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (24 24 (:REWRITE DEFAULT-CAR))
 (14 14 (:TYPE-PRESCRIPTION BITP))
 (12 12 (:REWRITE DEFAULT-CDR))
 )
(SATLINK::TRIVIAL-UNSAT-P1-OF-LIT-LIST-LIST-FIX-FORMULA
 (710 406 (:REWRITE DEFAULT-CDR))
 (550 106 (:REWRITE SATLINK::EQUAL-1-WHEN-BITP))
 (430 340 (:REWRITE DEFAULT-CAR))
 (362 66 (:REWRITE SATLINK::LIT-LIST-LIST-FIX-WHEN-LIT-LIST-LISTP))
 (212 212 (:TYPE-PRESCRIPTION BITP))
 (175 175 (:TYPE-PRESCRIPTION SATLINK::LIT-LIST-LISTP))
 (97 90 (:REWRITE SATLINK::LIT-LIST-LISTP-WHEN-NOT-CONSP))
 (96 24 (:REWRITE SATLINK::LIT-LIST-LISTP-OF-CDR-WHEN-LIT-LIST-LISTP))
 (95 95 (:REWRITE SATLINK::CLAUSE-UNIT-UNDER-ASSUMS-0-IMPLIES-UNSAT))
 (8 2 (:REWRITE SATLINK::LIT-LIST-FIX-WHEN-LIT-LISTP))
 (4 4 (:TYPE-PRESCRIPTION SATLINK::LIT-LISTP))
 (2 2 (:REWRITE SATLINK::LIT-LISTP-WHEN-NOT-CONSP))
 )
(SATLINK::TRIVIAL-UNSAT-P1-LIT-LIST-LIST-EQUIV-CONGRUENCE-ON-FORMULA)
(SATLINK::TRIVIAL-UNSAT-P
 (17 9 (:REWRITE DEFAULT-+-2))
 (10 9 (:REWRITE DEFAULT-+-1))
 (9 1 (:DEFINITION MAKE-LIST-AC-REDEF))
 (9 1 (:DEFINITION MAKE-LIST-AC))
 (6 3 (:REWRITE DEFAULT-*-2))
 (3 3 (:REWRITE SATLINK::LIT-LIST-LISTP-WHEN-NOT-CONSP))
 (3 3 (:REWRITE DEFAULT-*-1))
 (2 1 (:REWRITE NFIX-WHEN-NOT-NATP))
 (2 1 (:REWRITE DEFAULT-<-2))
 (2 1 (:REWRITE DEFAULT-<-1))
 )
(SATLINK::TRIVIAL-UNSAT-P-CORRECT
 (13 1 (:DEFINITION RESIZE-LIST))
 (7 7 (:TYPE-PRESCRIPTION SATLINK::NATP-OF-MAX-INDEX-FORMULA))
 (6 3 (:REWRITE DEFAULT-+-2))
 (4 1 (:REWRITE FOLD-CONSTS-IN-+))
 (3 3 (:REWRITE DEFAULT-+-1))
 (2 1 (:REWRITE DEFAULT-<-2))
 (2 1 (:REWRITE DEFAULT-*-2))
 (1 1 (:REWRITE DEFAULT-<-1))
 (1 1 (:REWRITE DEFAULT-*-1))
 )
(SATLINK::TRIVIAL-UNSAT-P-OF-LIT-LIST-LIST-FIX-FORMULA)
(SATLINK::TRIVIAL-UNSAT-P-LIT-LIST-LIST-EQUIV-CONGRUENCE-ON-FORMULA)
