(CLOSE-LAMBDAS
 (492 205 (:REWRITE DEFAULT-+-2))
 (288 205 (:REWRITE DEFAULT-+-1))
 (165 3 (:REWRITE ACL2-COUNT-WHEN-MEMBER))
 (144 36 (:DEFINITION INTEGER-ABS))
 (144 18 (:DEFINITION LENGTH))
 (126 6 (:REWRITE SUBSETP-CAR-MEMBER))
 (90 18 (:DEFINITION LEN))
 (87 3 (:DEFINITION MEMBER-EQUAL))
 (84 12 (:REWRITE SUBSETP-IMPLIES-SUBSETP-CDR))
 (60 43 (:REWRITE DEFAULT-<-2))
 (47 43 (:REWRITE DEFAULT-<-1))
 (36 36 (:TYPE-PRESCRIPTION SUBSETP-EQUAL))
 (36 36 (:REWRITE DEFAULT-UNARY-MINUS))
 (20 20 (:LINEAR ACL2-COUNT-WHEN-MEMBER))
 (18 18 (:TYPE-PRESCRIPTION LEN))
 (18 18 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (18 18 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (18 18 (:REWRITE SUBSETP-TRANS2))
 (18 18 (:REWRITE SUBSETP-TRANS))
 (18 18 (:REWRITE DEFAULT-REALPART))
 (18 18 (:REWRITE DEFAULT-NUMERATOR))
 (18 18 (:REWRITE DEFAULT-IMAGPART))
 (18 18 (:REWRITE DEFAULT-DENOMINATOR))
 (18 18 (:REWRITE DEFAULT-COERCE-2))
 (18 18 (:REWRITE DEFAULT-COERCE-1))
 (18 3 (:REWRITE SYMBOLP-OF-CAR-WHEN-SYMBOL-LISTP))
 (15 15 (:TYPE-PRESCRIPTION MEMBER-EQUAL))
 (8 8 (:REWRITE SYMBOLP-WHEN-MEMBER-EQUAL-OF-SYMBOL-LISTP))
 (6 6 (:REWRITE SYMBOL-LISTP-WHEN-SUBSETP-EQUAL))
 (6 6 (:REWRITE SUBSETP-MEMBER . 2))
 (6 6 (:REWRITE SUBSETP-MEMBER . 1))
 (5 5 (:LINEAR ACL2-COUNT-CAR-CDR-LINEAR))
 (3 3 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (1 1 (:TYPE-PRESCRIPTION CLOSE-LAMBDAS))
 )
(CLOSE-LAMBDAS-FLAG
 (634 273 (:REWRITE DEFAULT-+-2))
 (383 273 (:REWRITE DEFAULT-+-1))
 (200 50 (:DEFINITION INTEGER-ABS))
 (200 25 (:DEFINITION LENGTH))
 (125 25 (:DEFINITION LEN))
 (93 4 (:DEFINITION MEMBER-EQUAL))
 (84 12 (:REWRITE SUBSETP-IMPLIES-SUBSETP-CDR))
 (79 59 (:REWRITE DEFAULT-<-2))
 (65 59 (:REWRITE DEFAULT-<-1))
 (50 50 (:REWRITE DEFAULT-UNARY-MINUS))
 (25 25 (:TYPE-PRESCRIPTION LEN))
 (25 25 (:REWRITE DEFAULT-REALPART))
 (25 25 (:REWRITE DEFAULT-NUMERATOR))
 (25 25 (:REWRITE DEFAULT-IMAGPART))
 (25 25 (:REWRITE DEFAULT-DENOMINATOR))
 (25 25 (:REWRITE DEFAULT-COERCE-2))
 (25 25 (:REWRITE DEFAULT-COERCE-1))
 (24 2 (:TYPE-PRESCRIPTION RETURN-LAST))
 (22 22 (:LINEAR ACL2-COUNT-WHEN-MEMBER))
 (19 19 (:REWRITE SUBSETP-TRANS2))
 (19 19 (:REWRITE SUBSETP-TRANS))
 (18 18 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (18 18 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (18 3 (:REWRITE SYMBOLP-OF-CAR-WHEN-SYMBOL-LISTP))
 (8 8 (:REWRITE SYMBOLP-WHEN-MEMBER-EQUAL-OF-SYMBOL-LISTP))
 (8 8 (:REWRITE SUBSETP-MEMBER . 2))
 (8 8 (:REWRITE SUBSETP-MEMBER . 1))
 (6 6 (:REWRITE SYMBOL-LISTP-WHEN-SUBSETP-EQUAL))
 (3 3 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (2 2 (:TYPE-PRESCRIPTION THROW-NONEXEC-ERROR))
 (1 1 (:REWRITE MEMBER-SELF))
 )
(FLAG::FLAG-EQUIV-LEMMA)
(CLOSE-LAMBDAS-FLAG-EQUIVALENCES)
(FLAG-LEMMA-FOR-RETURN-TYPE-OF-CLOSE-LAMBDAS.NEW-TERM
 (6432 40 (:REWRITE PSEUDO-TERMP-OF-CAR-WHEN-PSEUDO-TERM-LISTP))
 (4502 822 (:REWRITE SUBSETP-IMPLIES-SUBSETP-CDR))
 (4165 179 (:DEFINITION MEMBER-EQUAL))
 (3873 131 (:REWRITE PSEUDO-TERM-LISTP-OF-CDR-WHEN-PSEUDO-TERM-LISTP))
 (2271 235 (:REWRITE SYMBOL-LISTP-OF-CDR-WHEN-SYMBOL-LISTP))
 (1762 1493 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (1676 1493 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (1548 1504 (:REWRITE SUBSETP-TRANS2))
 (1504 1504 (:REWRITE SUBSETP-TRANS))
 (1291 1137 (:REWRITE DEFAULT-CAR))
 (1172 1122 (:REWRITE DEFAULT-CDR))
 (794 122 (:REWRITE SYMBOLP-WHEN-MEMBER-EQUAL-OF-SYMBOL-LISTP))
 (753 86 (:REWRITE TRUE-LISTP-WHEN-SYMBOL-LISTP-REWRITE-BACKCHAIN-1))
 (732 573 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (608 86 (:REWRITE SET::SETS-ARE-TRUE-LISTS-CHEAP))
 (599 43 (:REWRITE SYMBOLP-OF-CAR-WHEN-SYMBOL-LISTP))
 (399 363 (:REWRITE SUBSETP-MEMBER . 1))
 (376 188 (:REWRITE DEFAULT-+-2))
 (363 363 (:REWRITE SUBSETP-MEMBER . 2))
 (250 250 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 (190 188 (:REWRITE DEFAULT-+-1))
 (172 172 (:TYPE-PRESCRIPTION SET::SETP-TYPE))
 (172 86 (:REWRITE SET::NONEMPTY-MEANS-SET))
 (165 1 (:REWRITE MEMBER-OF-APPEND))
 (144 1 (:REWRITE MEMBER-OF-SET-DIFFERENCE-EQUAL))
 (132 7 (:REWRITE SUBSETP-APPEND1))
 (123 3 (:DEFINITION SET-DIFFERENCE-EQUAL))
 (86 86 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (86 86 (:REWRITE SET::IN-SET))
 (79 79 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (66 2 (:REWRITE SUBSETP-OF-CONS))
 (54 13 (:REWRITE SUBSETP-MEMBER . 3))
 (39 3 (:DEFINITION ALL-VARS1))
 (24 3 (:DEFINITION ADD-TO-SET-EQUAL))
 (20 4 (:DEFINITION BINARY-APPEND))
 (15 1 (:REWRITE SYMBOL-LISTP-OF-CONS))
 (13 13 (:REWRITE SUBSETP-MEMBER . 4))
 (13 13 (:REWRITE MEMBER-WHEN-ATOM))
 (13 13 (:REWRITE INTERSECTP-MEMBER . 3))
 (13 13 (:REWRITE INTERSECTP-MEMBER . 2))
 (11 8 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (6 1 (:REWRITE CONSP-OF-APPEND))
 )
(RETURN-TYPE-OF-CLOSE-LAMBDAS.NEW-TERM)
(RETURN-TYPE-OF-CLOSE-LAMBDAS-LST.NEW-TERMS)
(CLOSE-LAMBDAS
 (464 37 (:REWRITE PSEUDO-TERM-LISTP-WHEN-SYMBOL-LISTP))
 (407 4 (:REWRITE PSEUDO-TERMP-OF-CAR-WHEN-PSEUDO-TERM-LISTP))
 (315 20 (:REWRITE PSEUDO-TERM-LISTP-OF-CDR-WHEN-PSEUDO-TERM-LISTP))
 (281 182 (:REWRITE SYMBOL-LISTP-WHEN-SUBSETP-EQUAL))
 (230 40 (:REWRITE SYMBOL-LISTP-OF-CDR-WHEN-SYMBOL-LISTP))
 (155 22 (:REWRITE TRUE-LISTP-WHEN-SYMBOL-LISTP-REWRITE-BACKCHAIN-1))
 (135 27 (:REWRITE SUBSETP-IMPLIES-SUBSETP-CDR))
 (127 22 (:REWRITE SET::SETS-ARE-TRUE-LISTS-CHEAP))
 (117 117 (:REWRITE DEFAULT-CAR))
 (112 112 (:REWRITE DEFAULT-CDR))
 (104 4 (:DEFINITION MEMBER-EQUAL))
 (97 91 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (88 1 (:DEFINITION CLOSE-LAMBDAS))
 (64 9 (:REWRITE SYMBOLP-OF-CAR-WHEN-SYMBOL-LISTP))
 (45 45 (:REWRITE SUBSETP-TRANS2))
 (45 45 (:REWRITE SUBSETP-TRANS))
 (44 44 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (44 44 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (42 42 (:TYPE-PRESCRIPTION SET::SETP-TYPE))
 (42 21 (:REWRITE SET::NONEMPTY-MEANS-SET))
 (41 1 (:DEFINITION SET-DIFFERENCE-EQUAL))
 (36 36 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 (36 36 (:REWRITE PSEUDO-TERM-LISTP-WHEN-NOT-CONSP))
 (31 31 (:TYPE-PRESCRIPTION CLOSE-LAMBDAS))
 (28 14 (:REWRITE DEFAULT-+-2))
 (26 2 (:DEFINITION ALL-VARS1))
 (21 21 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (21 21 (:REWRITE SET::IN-SET))
 (18 18 (:REWRITE SYMBOLP-WHEN-MEMBER-EQUAL-OF-SYMBOL-LISTP))
 (16 2 (:DEFINITION ADD-TO-SET-EQUAL))
 (14 14 (:REWRITE DEFAULT-+-1))
 (12 12 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (10 10 (:REWRITE SUBSETP-MEMBER . 2))
 (10 10 (:REWRITE SUBSETP-MEMBER . 1))
 (10 2 (:DEFINITION BINARY-APPEND))
 (7 4 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (4 4 (:REWRITE SUBSETP-MEMBER . 4))
 (4 4 (:REWRITE SUBSETP-MEMBER . 3))
 (4 4 (:REWRITE MEMBER-WHEN-ATOM))
 (4 4 (:REWRITE INTERSECTP-MEMBER . 3))
 (4 4 (:REWRITE INTERSECTP-MEMBER . 2))
 (3 1 (:DEFINITION CLOSE-LAMBDAS-LST))
 )
