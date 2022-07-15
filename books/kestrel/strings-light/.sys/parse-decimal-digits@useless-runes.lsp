(DECIMAL-DIGIT-CHAR-VALUE)
(NATP-OF-DECIMAL-DIGIT-CHAR-VALUE
 (1 1 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 )
(PARSE-DECIMAL-DIGIT-FROM-CHARS
 (251 251 (:REWRITE DEFAULT-CAR))
 (25 3 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (23 5 (:REWRITE DEFAULT-CDR))
 (10 4 (:LINEAR LEN-POSITIVE-WHEN-CONSP-LINEAR-CHEAP))
 (7 7 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (5 3 (:REWRITE DEFAULT-<-1))
 (4 3 (:REWRITE DEFAULT-<-2))
 (4 1 (:REWRITE CHARACTER-LISTP-OF-CDR))
 (3 3 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (2 2 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (1 1 (:REWRITE CONSP-WHEN-LEN-GREATER))
 )
(LEN-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS-STRONG-LINEAR
 (197 197 (:REWRITE DEFAULT-CAR))
 (175 175 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (157 84 (:REWRITE DEFAULT-<-2))
 (147 84 (:REWRITE DEFAULT-<-1))
 (100 11 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (23 23 (:REWRITE DEFAULT-CDR))
 (11 11 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (11 11 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (10 10 (:REWRITE DEFAULT-+-2))
 (10 10 (:REWRITE DEFAULT-+-1))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(LEN-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS-WEAK-LINEAR
 (260 260 (:REWRITE DEFAULT-CAR))
 (209 1 (:LINEAR LEN-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS-STRONG-LINEAR))
 (114 114 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (110 12 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (100 10 (:LINEAR LEN-OF-CDR-LINEAR-STRONG))
 (80 10 (:LINEAR LEN-OF-CDR-LINEAR))
 (40 40 (:REWRITE DEFAULT-+-2))
 (40 40 (:REWRITE DEFAULT-+-1))
 (38 25 (:REWRITE DEFAULT-<-2))
 (36 36 (:REWRITE DEFAULT-CDR))
 (28 25 (:REWRITE DEFAULT-<-1))
 (12 12 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (12 12 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (2 2 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(NATP-OF-MV-NTH-0-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS
 (197 197 (:REWRITE DEFAULT-CAR))
 (100 11 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (23 12 (:REWRITE DEFAULT-<-2))
 (21 21 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (13 13 (:REWRITE DEFAULT-CDR))
 (13 12 (:REWRITE DEFAULT-<-1))
 (11 11 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (11 11 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(NATP-OF-MV-NTH-0-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS-TYPE
 (375 375 (:REWRITE DEFAULT-CAR))
 (54 6 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (36 9 (:REWRITE DEFAULT-CDR))
 (27 27 (:TYPE-PRESCRIPTION LEN))
 (12 6 (:LINEAR LEN-POSITIVE-WHEN-CONSP-LINEAR-CHEAP))
 (9 9 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (9 6 (:REWRITE DEFAULT-<-2))
 (9 6 (:REWRITE DEFAULT-<-1))
 (6 6 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (3 3 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (3 3 (:REWRITE CONSP-WHEN-LEN-GREATER))
 )
(CHARACTER-LISTP-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS
 (127 127 (:REWRITE DEFAULT-CAR))
 (38 2 (:DEFINITION CHARACTER-LISTP))
 (30 3 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (15 15 (:TYPE-PRESCRIPTION LEN))
 (8 2 (:REWRITE CHARACTER-LISTP-OF-CDR))
 (7 4 (:REWRITE DEFAULT-<-2))
 (5 5 (:REWRITE DEFAULT-CDR))
 (5 4 (:REWRITE DEFAULT-<-1))
 (3 3 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (3 3 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (3 3 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (3 3 (:LINEAR LEN-POSITIVE-WHEN-CONSP-LINEAR-CHEAP))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(TRUE-LISTP-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS
 (125 125 (:REWRITE DEFAULT-CAR))
 (30 3 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (28 2 (:DEFINITION TRUE-LISTP))
 (15 15 (:TYPE-PRESCRIPTION LEN))
 (7 4 (:REWRITE DEFAULT-<-2))
 (5 5 (:REWRITE DEFAULT-CDR))
 (5 4 (:REWRITE DEFAULT-<-1))
 (3 3 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (3 3 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (3 3 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (3 3 (:LINEAR LEN-POSITIVE-WHEN-CONSP-LINEAR-CHEAP))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(PARSE-DECIMAL-DIGITS-FROM-CHARS
 (19 1 (:DEFINITION CHARACTER-LISTP))
 (14 8 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (12 6 (:TYPE-PRESCRIPTION TRUE-LISTP-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS))
 (10 1 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (6 6 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (4 2 (:REWRITE DEFAULT-<-2))
 (4 1 (:REWRITE CHARACTER-LISTP-OF-CDR))
 (3 2 (:REWRITE DEFAULT-<-1))
 (2 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:REWRITE DEFAULT-+-1))
 (2 2 (:REWRITE DEFAULT-*-2))
 (2 2 (:REWRITE DEFAULT-*-1))
 (1 1 (:REWRITE DEFAULT-CDR))
 (1 1 (:REWRITE DEFAULT-CAR))
 (1 1 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (1 1 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (1 1 (:LINEAR LEN-POSITIVE-WHEN-CONSP-LINEAR-CHEAP))
 )
(NATP-OF-MV-NTH-0-OF-PARSE-DECIMAL-DIGITS-FROM-CHARS
 (12 4 (:REWRITE COMMUTATIVITY-OF-+))
 (8 8 (:REWRITE DEFAULT-+-2))
 (8 8 (:REWRITE DEFAULT-+-1))
 (4 4 (:REWRITE DEFAULT-*-2))
 (4 4 (:REWRITE DEFAULT-*-1))
 )
(NATP-OF-MV-NTH-0-OF-PARSE-DECIMAL-DIGITS-FROM-CHARS-TYPE
 (27 3 (:DEFINITION PARSE-DECIMAL-DIGITS-FROM-CHARS))
 (9 3 (:REWRITE COMMUTATIVITY-OF-+))
 (6 6 (:REWRITE DEFAULT-+-2))
 (6 6 (:REWRITE DEFAULT-+-1))
 (3 3 (:TYPE-PRESCRIPTION NATP-OF-MV-NTH-0-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS-TYPE))
 (3 3 (:REWRITE DEFAULT-*-2))
 (3 3 (:REWRITE DEFAULT-*-1))
 )
(LEN-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGITS-FROM-CHARS-WEAK-LINEAR
 (32 20 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (20 10 (:TYPE-PRESCRIPTION TRUE-LISTP-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS))
 (12 6 (:REWRITE DEFAULT-<-2))
 (12 6 (:REWRITE DEFAULT-<-1))
 (10 10 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (10 8 (:LINEAR LEN-POSITIVE-WHEN-CONSP-LINEAR-CHEAP))
 (6 2 (:REWRITE COMMUTATIVITY-OF-+))
 (4 4 (:REWRITE DEFAULT-+-2))
 (4 4 (:REWRITE DEFAULT-+-1))
 (4 2 (:REWRITE DEFAULT-*-2))
 (2 2 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (2 2 (:REWRITE DEFAULT-*-1))
 )
(TRUE-LISTP-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGITS-FROM-CHARS
 (187 9 (:DEFINITION TRUE-LISTP))
 (151 9 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (65 65 (:TYPE-PRESCRIPTION LEN))
 (40 3 (:LINEAR LEN-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGITS-FROM-CHARS-WEAK-LINEAR))
 (22 19 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (18 9 (:REWRITE DEFAULT-<-2))
 (14 13 (:LINEAR LEN-POSITIVE-WHEN-CONSP-LINEAR-CHEAP))
 (12 4 (:REWRITE COMMUTATIVITY-OF-+))
 (9 9 (:REWRITE DEFAULT-CDR))
 (9 9 (:REWRITE DEFAULT-<-1))
 (9 9 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (9 9 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (8 8 (:REWRITE DEFAULT-+-2))
 (8 8 (:REWRITE DEFAULT-+-1))
 (8 1 (:LINEAR LEN-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS-WEAK-LINEAR))
 (8 1 (:LINEAR LEN-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS-STRONG-LINEAR))
 (6 4 (:REWRITE DEFAULT-*-2))
 (4 4 (:REWRITE DEFAULT-*-1))
 (2 2 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(CHARACTER-LISTP-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGITS-FROM-CHARS
 (310 15 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (83 49 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (72 8 (:REWRITE COMMUTATIVITY-2-OF-+))
 (59 8 (:REWRITE DISTRIBUTIVITY))
 (56 56 (:REWRITE DEFAULT-+-2))
 (56 56 (:REWRITE DEFAULT-+-1))
 (38 32 (:REWRITE DEFAULT-*-2))
 (32 32 (:REWRITE DEFAULT-*-1))
 (24 12 (:REWRITE DEFAULT-<-2))
 (19 14 (:REWRITE DEFAULT-CAR))
 (17 12 (:REWRITE DEFAULT-CDR))
 (16 16 (:REWRITE FOLD-CONSTS-IN-+))
 (15 15 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (12 12 (:REWRITE DEFAULT-<-1))
 (12 12 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (6 6 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(PARSE-DECIMAL-NUMBER-FROM-CHARS
 (19 1 (:DEFINITION CHARACTER-LISTP))
 (10 1 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (5 5 (:TYPE-PRESCRIPTION LEN))
 (4 1 (:REWRITE CHARACTER-LISTP-OF-CDR))
 (2 1 (:REWRITE DEFAULT-<-2))
 (1 1 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (1 1 (:REWRITE DEFAULT-CDR))
 (1 1 (:REWRITE DEFAULT-CAR))
 (1 1 (:REWRITE DEFAULT-<-1))
 (1 1 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (1 1 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (1 1 (:LINEAR LEN-POSITIVE-WHEN-CONSP-LINEAR-CHEAP))
 )
(PARSE-DECIMAL-NUMBER-FROM-CHARS-LEN-BOUND
 (20 10 (:TYPE-PRESCRIPTION TRUE-LISTP-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS))
 (20 8 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (18 3 (:TYPE-PRESCRIPTION TRUE-LISTP-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGITS-FROM-CHARS))
 (13 13 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (8 2 (:LINEAR LEN-POSITIVE-WHEN-CONSP-LINEAR-CHEAP))
 (2 1 (:REWRITE DEFAULT-<-2))
 (2 1 (:REWRITE DEFAULT-<-1))
 )
(TRUE-LISTP-OF-MV-NTH-1-OF-PARSE-DECIMAL-NUMBER-FROM-CHARS
 (28 2 (:DEFINITION TRUE-LISTP))
 (20 2 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (10 10 (:TYPE-PRESCRIPTION LEN))
 (4 2 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (2 2 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE DEFAULT-<-1))
 (2 2 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (2 2 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (2 2 (:LINEAR LEN-POSITIVE-WHEN-CONSP-LINEAR-CHEAP))
 )
(CHARACTER-LISTP-OF-MV-NTH-1-OF-PARSE-DECIMAL-NUMBER-FROM-CHARS
 (38 2 (:DEFINITION CHARACTER-LISTP))
 (20 2 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (10 10 (:TYPE-PRESCRIPTION LEN))
 (8 2 (:TYPE-PRESCRIPTION CHARACTER-LISTP-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGITS-FROM-CHARS))
 (8 2 (:REWRITE CHARACTER-LISTP-OF-CDR))
 (4 2 (:TYPE-PRESCRIPTION TRUE-LISTP-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS))
 (4 2 (:REWRITE DEFAULT-<-2))
 (2 2 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (2 2 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (2 2 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE DEFAULT-<-1))
 (2 2 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (2 2 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (2 2 (:LINEAR LEN-POSITIVE-WHEN-CONSP-LINEAR-CHEAP))
 )
(NATP-OF-MV-NTH-0-OF-PARSE-DECIMAL-NUMBER-FROM-CHARS-TYPE
 (2 1 (:TYPE-PRESCRIPTION TRUE-LISTP-OF-MV-NTH-1-OF-PARSE-DECIMAL-DIGIT-FROM-CHARS))
 (1 1 (:TYPE-PRESCRIPTION TRUE-LISTP))
 )
(NATP-OF-MV-NTH-0-OF-PARSE-DECIMAL-NUMBER-FROM-CHARS)