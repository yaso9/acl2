(EDP)
(=_E)
(+_E)
(*_E)
(-_E)
(|0_E|)
(SIZE)
(Q_E)
(R_E)
(CLOSURE-LAWS)
(EQUIVALENCE-LAW)
(CONGRUENCE-LAWS)
(COMMUTATIVITY-LAWS
 (3 3 (:REWRITE DEFAULT-+-2))
 (3 3 (:REWRITE DEFAULT-+-1))
 (3 3 (:REWRITE DEFAULT-*-2))
 (3 3 (:REWRITE DEFAULT-*-1))
 )
(ASSOCIATIVITY-LAWS)
(LEFT-DISTRIBUTIVITY-LAW
 (6 2 (:LINEAR X*Y>1-POSITIVE))
 (5 5 (:REWRITE DEFAULT-*-2))
 (5 5 (:REWRITE DEFAULT-*-1))
 (3 3 (:REWRITE DEFAULT-+-2))
 (3 3 (:REWRITE DEFAULT-+-1))
 (2 2 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE DEFAULT-<-1))
 )
(LEFT-UNICITY-LAW)
(RIGHT-INVERSE-LAW)
(ZERO-DIVISOR-LAW
 (1 1 (:REWRITE DEFAULT-*-2))
 (1 1 (:REWRITE DEFAULT-*-1))
 )
(NATP-SIZE)
(CONGRUENCE-FOR-SIZE
 (4 2 (:TYPE-PRESCRIPTION NATP-SIZE))
 )
(CLOSURE-OF-Q_E-&-R_E)
(CONGRUENCE-FOR-Q_E-&-R_E)
(DIVISION-PROPERTY
 (6 2 (:REWRITE /R-WHEN-ABS-NUMERATOR=1))
 (6 2 (:LINEAR X*Y>1-POSITIVE))
 (2 2 (:REWRITE NUMERATOR-WHEN-INTEGERP))
 (2 2 (:REWRITE DEFAULT-NUMERATOR))
 (2 2 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE DEFAULT-<-1))
 (2 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:REWRITE DEFAULT-+-1))
 (1 1 (:REWRITE FOLD-CONSTS-IN-*))
 (1 1 (:REWRITE DEFAULT-UNARY-/))
 (1 1 (:REWRITE DEFAULT-*-2))
 (1 1 (:REWRITE DEFAULT-*-1))
 )
(==_E)
(++_E)
(**_E)
(CLOSURE-LAWS-FOR-++_E-&-**_E)
(==_E-EQUIVALENCE-LAW
 (86 86 (:REWRITE CLOSURE-LAWS))
 )
(==_E-IMPLIES-IFF-EDP
 (43 24 (:REWRITE EQUIVALENCE-LAW))
 (19 19 (:REWRITE CLOSURE-LAWS))
 )
(==_E-IMPLIES-==_E-++_E-1
 (1410 16 (:REWRITE ZERO-DIVISOR-LAW))
 )
(==_E-IMPLIES-==_E-++_E-2
 (1434 16 (:REWRITE ZERO-DIVISOR-LAW))
 )
(==_E-IMPLIES-==_E-**_E-1
 (2820 32 (:REWRITE ZERO-DIVISOR-LAW))
 )
(==_E-IMPLIES-==_E-**_E-2
 (2868 32 (:REWRITE ZERO-DIVISOR-LAW))
 )
(COMMUTATIVITY-LAWS-FOR-++_E-&-**_E)
(ASSOCIATIVITY-LAWS-FOR-++_E-&-**_E)
(LEFT-DISTRIBUTIVITY-LAW-FOR-++_E-&-**_E)
(LEFT-UNICITY-LAW-FOR-++_E)
(RIGHT-INVERSE-LAW-FOR-++_E)
(ZERO-DIVISOR-LAW-FOR-**_E
 (114 56 (:REWRITE EQUIVALENCE-LAW))
 )
(NATP-SIZE-FOR-==_E
 (46 22 (:REWRITE EQUIVALENCE-LAW))
 )
(CONGRUENCE-FOR-SIZE-&-==_E
 (68 58 (:TYPE-PRESCRIPTION NATP-SIZE-FOR-==_E))
 (66 36 (:REWRITE EQUIVALENCE-LAW))
 (58 58 (:TYPE-PRESCRIPTION NATP-SIZE))
 (30 30 (:REWRITE CLOSURE-LAWS))
 )
(CLOSURE-OF-Q_E-&-R_E-FOR-==_E
 (57 28 (:REWRITE EQUIVALENCE-LAW))
 )
(DIVISION-PROPERTY-FOR-==_E
 (28 2 (:REWRITE DEFAULT-<-1))
 (14 2 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (6 2 (:REWRITE DEFAULT-<-2))
 )
(A_E-PROPERTY)
(A_E-PROPERTY-1)
(NATP-SIZE-A_E
 (12 12 (:TYPE-PRESCRIPTION NATP-SIZE))
 (1 1 (:TYPE-PRESCRIPTION NATP-SIZE-A_E))
 )
(B_E-PROPERTY
 (9 9 (:TYPE-PRESCRIPTION NATP-SIZE))
 (2 2 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE DEFAULT-<-1))
 )
(B_E-PROPERTIES-SIZE
 (9 3 (:REWRITE B_E-PROPERTY))
 (4 2 (:REWRITE DEFAULT-<-1))
 (2 2 (:REWRITE DEFAULT-<-2))
 (1 1 (:TYPE-PRESCRIPTION NATP-SIZE))
 )
(FIND-SMALLEST-N
 (82 48 (:REWRITE DEFAULT-<-1))
 (65 46 (:REWRITE DEFAULT-+-1))
 (64 64 (:TYPE-PRESCRIPTION NATP-SIZE))
 (55 46 (:REWRITE DEFAULT-+-2))
 (52 48 (:REWRITE DEFAULT-<-2))
 (20 2 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (10 10 (:REWRITE DEFAULT-UNARY-MINUS))
 (9 9 (:REWRITE B_E-PROPERTY))
 (6 6 (:REWRITE FOLD-CONSTS-IN-+))
 (3 3 (:REWRITE A_E-PROPERTY))
 (1 1 (:TYPE-PRESCRIPTION FIND-SMALLEST-N))
 )
(INTEGERP-FIND-SMALLEST-N)
(NATP-FIND-SMALLEST-N
 (133 77 (:REWRITE DEFAULT-<-1))
 (118 118 (:TYPE-PRESCRIPTION NATP-SIZE))
 (89 77 (:REWRITE DEFAULT-<-2))
 (54 18 (:REWRITE COMMUTATIVITY-OF-+))
 (36 36 (:REWRITE DEFAULT-+-2))
 (36 36 (:REWRITE DEFAULT-+-1))
 (14 2 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(FIND-SMALLEST-N-SIZE-A_E
 (388 388 (:TYPE-PRESCRIPTION NATP-SIZE))
 (303 154 (:REWRITE DEFAULT-<-1))
 (247 154 (:REWRITE DEFAULT-<-2))
 (128 128 (:REWRITE DEFAULT-+-2))
 (128 128 (:REWRITE DEFAULT-+-1))
 (108 24 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (90 30 (:REWRITE FOLD-CONSTS-IN-+))
 (10 10 (:REWRITE EQUAL-CONSTANT-+))
 )
(B_E-FIND-SMALLEST-N
 (1079 1079 (:TYPE-PRESCRIPTION NATP-SIZE))
 (973 973 (:REWRITE DEFAULT-+-2))
 (973 973 (:REWRITE DEFAULT-+-1))
 (693 231 (:REWRITE FOLD-CONSTS-IN-+))
 (443 372 (:REWRITE DEFAULT-<-2))
 (385 372 (:REWRITE DEFAULT-<-1))
 (77 77 (:REWRITE EQUAL-CONSTANT-+))
 )
(NOT-SIZE-B_E-N-=-N
 (456 456 (:TYPE-PRESCRIPTION NATP-SIZE))
 (247 247 (:REWRITE DEFAULT-+-2))
 (247 247 (:REWRITE DEFAULT-+-1))
 (218 154 (:REWRITE DEFAULT-<-2))
 (154 154 (:REWRITE DEFAULT-<-1))
 (153 51 (:REWRITE FOLD-CONSTS-IN-+))
 (17 17 (:REWRITE EQUAL-CONSTANT-+))
 (4 2 (:REWRITE B_E-PROPERTIES-SIZE))
 )
(NATP-FIND-SMALLEST-N-0)
(B_E-FIND-SMALLEST-N-0
 (70 5 (:DEFINITION FIND-SMALLEST-N))
 (15 15 (:REWRITE B_E-PROPERTY))
 (10 10 (:REWRITE A_E-PROPERTY))
 (10 5 (:REWRITE DEFAULT-<-2))
 (5 5 (:REWRITE NOT-SIZE-B_E-N-=-N))
 (5 5 (:REWRITE DEFAULT-<-1))
 )
(NOT-SIZE-B_E-N-=-N-1)
(SIZE->=-FIND-SMALLEST-N-0
 (14 1 (:DEFINITION FIND-SMALLEST-N))
 (8 8 (:TYPE-PRESCRIPTION NATP-SIZE))
 (5 3 (:REWRITE DEFAULT-<-1))
 (4 3 (:REWRITE DEFAULT-<-2))
 (3 3 (:TYPE-PRESCRIPTION NATP-SIZE-A_E))
 (3 3 (:REWRITE B_E-PROPERTY))
 (2 2 (:REWRITE A_E-PROPERTY))
 (1 1 (:REWRITE NOT-SIZE-B_E-N-=-N))
 )
(B_E-FIND-SMALLEST-N-0-DIVIDES-Y
 (234 15 (:DEFINITION FIND-SMALLEST-N))
 (54 30 (:REWRITE A_E-PROPERTY))
 (45 45 (:TYPE-PRESCRIPTION NATP-SIZE-A_E))
 (45 45 (:REWRITE B_E-PROPERTY))
 (30 15 (:REWRITE DEFAULT-<-2))
 (18 18 (:TYPE-PRESCRIPTION NATP-SIZE))
 (15 15 (:REWRITE NOT-SIZE-B_E-N-=-N))
 (15 15 (:REWRITE DEFAULT-<-1))
 )
(|1_E|)
(CLOSURE-LAW-FOR-1-E
 (56 4 (:DEFINITION FIND-SMALLEST-N))
 (13 13 (:REWRITE A_E-PROPERTY))
 (12 12 (:TYPE-PRESCRIPTION NATP-SIZE-A_E))
 (12 12 (:REWRITE B_E-PROPERTY))
 (8 8 (:TYPE-PRESCRIPTION NATP-SIZE-FOR-==_E))
 (8 4 (:REWRITE DEFAULT-<-2))
 (4 4 (:REWRITE NOT-SIZE-B_E-N-=-N))
 (4 4 (:REWRITE DEFAULT-<-1))
 )
(COMMUTATIVITY-2-LAWS)
(NULLITY-LAWS)
(ASSOCIATIVITY-SPECIAL-CASE
 (112 8 (:DEFINITION FIND-SMALLEST-N))
 (24 24 (:TYPE-PRESCRIPTION NATP-SIZE-A_E))
 (24 24 (:REWRITE B_E-PROPERTY))
 (19 19 (:REWRITE A_E-PROPERTY))
 (16 16 (:TYPE-PRESCRIPTION NATP-SIZE-FOR-==_E))
 (16 8 (:REWRITE DEFAULT-<-2))
 (8 8 (:REWRITE NOT-SIZE-B_E-N-=-N))
 (8 8 (:REWRITE DEFAULT-<-1))
 )
(LEFT-UNICITY-LAW-FOR-1_E-==_E
 (140 10 (:DEFINITION FIND-SMALLEST-N))
 (30 30 (:TYPE-PRESCRIPTION NATP-SIZE-A_E))
 (30 30 (:REWRITE B_E-PROPERTY))
 (20 20 (:TYPE-PRESCRIPTION NATP-SIZE-FOR-==_E))
 (20 10 (:REWRITE DEFAULT-<-2))
 (10 10 (:REWRITE NOT-SIZE-B_E-N-=-N))
 (10 10 (:REWRITE DEFAULT-<-1))
 )
(LEFT-UNICITY-LAW-FOR-1_E
 (222 75 (:REWRITE EQUIVALENCE-LAW))
 (166 1 (:REWRITE ZERO-DIVISOR-LAW))
 )
