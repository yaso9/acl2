(DJVM::ISASSIGNABLE-TO-ARRAY-CLASS-JAVA-LANG-OBJECT-IMPLIES-NOT-DEREF2-INIT-SPECIFIC
 (1148 14 (:DEFINITION DJVM::OPSTACK-SIG))
 (1079 2 (:REWRITE DJVM::TOPSTACK-GUARD-STRONG-IMPLIED-BY-CANPOPCATEGORY1))
 (988 1 (:REWRITE DJVM::CONSISTENT-STATE-TOPSTACK-GUARD-STRONG-IMPLIES-CONSISTENT-VALUE-X-B))
 (825 16 (:REWRITE DJVM::ISMATCHINGTYPE-CLASS-OBJECT-IMPLIES-CANPOPCATEGORY1-SPECIFIC-B))
 (704 12 (:REWRITE BCV::CANPOP1-IMPLIES-BCV-ISMATCHINGTYPE-SPECIFIC))
 (584 16 (:DEFINITION BCV::CANPOP1))
 (266 7 (:DEFINITION DJVM::CANPOPCATEGORY2))
 (260 1 (:REWRITE DJVM::BCV-ISMATCHINGTYPE-BCV-ISASSIGNABLE-SPECIFIC))
 (214 9 (:DEFINITION DJVM::CANPOPCATEGORY1))
 (211 22 (:REWRITE DJVM::VALUE-SIG-BEING-FIX-SIG-NULL-SHORT-CUT))
 (180 16 (:DEFINITION LEN))
 (172 44 (:REWRITE DJVM::NULLP-IMPLIED-BY))
 (172 9 (:REWRITE DJVM::ISMATCHINGTYPE-AARRAY-IMPLIES-CANPOPCATEGORY1-SPECIFIC-B))
 (140 140 (:TYPE-PRESCRIPTION DJVM::WFF-REFP))
 (129 20 (:REWRITE DJVM::TOPSTACK-GUARD-STRONG-IMPLIES-CONSP-OPSTACK))
 (127 9 (:REWRITE DJVM::ISMATCHINGTYPE-INT-IMPLIES-CANPOPCATEGORY1-SPECIFIC-B))
 (118 118 (:TYPE-PRESCRIPTION DJVM::CONSISTENT-STATE))
 (112 16 (:REWRITE DJVM::FRAME-POP-OPSTACK-NORMALIZE))
 (105 35 (:REWRITE DJVM::WFF-REFP-IMPLIES-TAG-OF-EQUAL-REF))
 (105 35 (:REWRITE DJVM::WFF-REFP-IMPLIES-TAG-OF-BEING-REF))
 (105 7 (:DEFINITION DJVM::CATEGORY2))
 (104 32 (:REWRITE DJVM::ISMATCHINGTYPE-POPMATCHINGTYPE-FORM1))
 (98 98 (:TYPE-PRESCRIPTION LEN))
 (88 44 (:DEFINITION JVM::TOP))
 (85 22 (:REWRITE DJVM::VALUE-SIG-NULLP))
 (48 16 (:REWRITE DJVM::FRAME-POP-OPSTACK-CURRENT-FRAME))
 (48 16 (:REWRITE DJVM::CURRENT-FRAME-POPSTACK))
 (42 14 (:DEFINITION DJVM::TOPCATEGORY1))
 (37 37 (:REWRITE DJVM::FRAME-TOP-OPSTACK-NORMALIZE))
 (36 36 (:TYPE-PRESCRIPTION BCV::CANPOP1))
 (32 16 (:REWRITE DEFAULT-<-1))
 (32 16 (:REWRITE DEFAULT-+-2))
 (28 28 (:DEFINITION JVM::PUSH))
 (24 24 (:TYPE-PRESCRIPTION BCV::SIZEOF))
 (24 24 (:TYPE-PRESCRIPTION DJVM::OPSTACK-SIG))
 (23 23 (:TYPE-PRESCRIPTION DJVM::TOPSTACK-GUARD-STRONG))
 (22 22 (:REWRITE BCV::CANPOP1-IMPLIES-BCV-ISMATCHINGTYPE))
 (21 21 (:REWRITE DJVM::ISMATCHINGTYPE-INT-IMPLIES-VALUE-SIG-INT-SPECIFIC))
 (21 7 (:DEFINITION DJVM::TOPCATEGORY2))
 (16 16 (:REWRITE DEL-SET-LEN))
 (16 16 (:REWRITE DEFAULT-<-2))
 (16 16 (:REWRITE DEFAULT-+-1))
 (14 14 (:DEFINITION DJVM::POPCATEGORY1))
 (14 2 (:REWRITE DJVM::NOT-NULLP-IMPLIED-BY-NOT-EQUAL))
 (13 3 (:REWRITE DJVM::CHECK-NULL-NOT-EQUAL-VALUE-OF-MINUS-1))
 (10 1 (:REWRITE DJVM::CHECK-NULL-IMPLIES-VALUE-BEING-NEGATIVE-1))
 (9 9 (:TYPE-PRESCRIPTION DJVM::CATEGORY1))
 (9 3 (:REWRITE DJVM::NOT-NULLP-TAG-REF-NOT-EQUAL-MINUS-1))
 (7 7 (:REWRITE DEFAULT-CAR))
 (7 7 (:REWRITE DJVM::CONSISTENT-VALUE-X-IMPLIES-WFF-TAGGED-VALUE-B))
 (7 7 (:DEFINITION DJVM::POPCATEGORY2))
 (6 2 (:LINEAR DJVM::CONSP-LEN-NO-LESS-THAN-0))
 (6 2 (:LINEAR DJVM::CONSP-IMPLIES-LEN))
 (6 2 (:LINEAR DJVM::CONSISTENT-STATE-IMPLIES-LEN-OPSTACK-LESS-MAX-STACK-LINEAR))
 (4 4 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (4 2 (:REWRITE DJVM::NOT-VALUE-OF-V-IMPLIES-NOT-NULLP))
 (3 1 (:REWRITE DJVM::CONSISTENT-STATE-CONSISTENT-VALUE-X))
 (2 2 (:TYPE-PRESCRIPTION DJVM::CANPOPCATEGORY1))
 (2 1 (:REWRITE DJVM::REFP-IMPLIES-NOT-TAG-OF-SPECIFIC))
 (1 1 (:REWRITE DJVM::VALUE-SIG-ISASSIGNABLE-TO-ARRRAY-IMPLIES-BEING-REFP-B))
 (1 1 (:REWRITE DJVM::TAG-REF-TAG))
 (1 1 (:REWRITE DJVM::ISMATCHINGTYPE-INT-IMPLIES-VALUE-SIG-INT-SPECIFIC-2))
 (1 1 (:REWRITE DJVM::CONSISTENT-VALUE-IMPLIES-CONSISTENT-VALUE-X-B))
 )
(DJVM::ISMATCHINGTYPE-TO-ARRAY-CLASS-JAVA-LANG-OBJECT-IMPLIES-NOT-DEREF2-INIT-SPECIFIC
 (260 1 (:DEFINITION DJVM::OPSTACK-SIG))
 (104 2 (:REWRITE DJVM::ISMATCHINGTYPE-CLASS-OBJECT-IMPLIES-CANPOPCATEGORY1-SPECIFIC-B))
 (100 6 (:REWRITE BCV::CANPOP1-IMPLIES-BCV-ISMATCHINGTYPE-SPECIFIC))
 (98 4 (:REWRITE DJVM::TOPSTACK-GUARD-STRONG-IMPLIES-CONSP-OPSTACK))
 (98 2 (:REWRITE DJVM::TOPSTACK-GUARD-STRONG-IMPLIED-BY-CANPOPCATEGORY1))
 (96 8 (:DEFINITION BCV::CANPOP1))
 (57 2 (:DEFINITION DJVM::CANPOPCATEGORY1))
 (52 16 (:REWRITE DJVM::ISMATCHINGTYPE-POPMATCHINGTYPE-FORM1))
 (42 42 (:TYPE-PRESCRIPTION DJVM::CONSISTENT-STATE))
 (38 1 (:DEFINITION DJVM::CANPOPCATEGORY2))
 (35 3 (:DEFINITION LEN))
 (31 8 (:REWRITE DJVM::NULLP-IMPLIED-BY))
 (25 4 (:REWRITE DJVM::VALUE-SIG-BEING-FIX-SIG-NULL-SHORT-CUT))
 (21 3 (:REWRITE DJVM::FRAME-POP-OPSTACK-NORMALIZE))
 (19 19 (:TYPE-PRESCRIPTION LEN))
 (18 18 (:TYPE-PRESCRIPTION BCV::CANPOP1))
 (16 16 (:TYPE-PRESCRIPTION DJVM::WFF-REFP))
 (15 1 (:DEFINITION DJVM::CATEGORY2))
 (14 2 (:REWRITE DJVM::NOT-NULLP-IMPLIED-BY-NOT-EQUAL))
 (13 4 (:REWRITE DJVM::VALUE-SIG-NULLP))
 (13 3 (:REWRITE DJVM::CHECK-NULL-NOT-EQUAL-VALUE-OF-MINUS-1))
 (13 1 (:REWRITE DJVM::REFP-IMPLIES-NOT-TAG-OF-SPECIFIC))
 (12 12 (:TYPE-PRESCRIPTION DJVM::OPSTACK-SIG))
 (12 6 (:DEFINITION JVM::TOP))
 (12 4 (:REWRITE DJVM::WFF-REFP-IMPLIES-TAG-OF-EQUAL-REF))
 (12 4 (:REWRITE DJVM::WFF-REFP-IMPLIES-TAG-OF-BEING-REF))
 (12 1 (:REWRITE DJVM::VALUE-SIG-ISASSIGNABLE-TO-ARRRAY-IMPLIES-BEING-REFP-B))
 (11 11 (:REWRITE BCV::CANPOP1-IMPLIES-BCV-ISMATCHINGTYPE))
 (10 1 (:REWRITE DJVM::CHECK-NULL-IMPLIES-VALUE-BEING-NEGATIVE-1))
 (9 3 (:REWRITE DJVM::NOT-NULLP-TAG-REF-NOT-EQUAL-MINUS-1))
 (9 3 (:REWRITE DJVM::FRAME-POP-OPSTACK-CURRENT-FRAME))
 (9 3 (:REWRITE DJVM::CURRENT-FRAME-POPSTACK))
 (7 7 (:TYPE-PRESCRIPTION DJVM::TOPSTACK-GUARD-STRONG))
 (6 3 (:REWRITE DEFAULT-<-1))
 (6 3 (:REWRITE DEFAULT-+-2))
 (6 1 (:REWRITE DJVM::CONSISTENT-STATE-TOPSTACK-GUARD-STRONG-IMPLIES-CONSISTENT-VALUE-X-B))
 (5 5 (:REWRITE DJVM::FRAME-TOP-OPSTACK-NORMALIZE))
 (4 2 (:REWRITE DJVM::NOT-VALUE-OF-V-IMPLIES-NOT-NULLP))
 (4 2 (:REWRITE DJVM::ISMATCHINGTYPE-INT-IMPLIES-CANPOPCATEGORY1-SPECIFIC-B))
 (4 2 (:REWRITE DJVM::ISMATCHINGTYPE-AARRAY-IMPLIES-CANPOPCATEGORY1-SPECIFIC-B))
 (3 3 (:REWRITE DEL-SET-LEN))
 (3 3 (:REWRITE DEFAULT-<-2))
 (3 3 (:REWRITE DEFAULT-+-1))
 (3 3 (:DEFINITION JVM::PUSH))
 (3 1 (:REWRITE DJVM::CONSISTENT-STATE-CONSISTENT-VALUE-X))
 (3 1 (:LINEAR DJVM::CONSP-LEN-NO-LESS-THAN-0))
 (3 1 (:LINEAR DJVM::CONSP-IMPLIES-LEN))
 (3 1 (:LINEAR DJVM::CONSISTENT-STATE-IMPLIES-LEN-OPSTACK-LESS-MAX-STACK-LINEAR))
 (3 1 (:DEFINITION DJVM::TOPCATEGORY2))
 (3 1 (:DEFINITION DJVM::TOPCATEGORY1))
 (2 2 (:TYPE-PRESCRIPTION DJVM::CATEGORY1))
 (2 2 (:REWRITE DJVM::ISMATCHINGTYPE-INT-IMPLIES-VALUE-SIG-INT-SPECIFIC-2))
 (2 2 (:REWRITE DJVM::ISMATCHINGTYPE-INT-IMPLIES-VALUE-SIG-INT-SPECIFIC))
 (2 2 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (1 1 (:REWRITE DJVM::TAG-REF-TAG))
 (1 1 (:REWRITE DEFAULT-CAR))
 (1 1 (:REWRITE DJVM::CONSISTENT-VALUE-X-IMPLIES-WFF-TAGGED-VALUE-B))
 (1 1 (:REWRITE DJVM::CONSISTENT-VALUE-IMPLIES-CONSISTENT-VALUE-X-B))
 (1 1 (:DEFINITION DJVM::POPCATEGORY2))
 (1 1 (:DEFINITION DJVM::POPCATEGORY1))
 )
