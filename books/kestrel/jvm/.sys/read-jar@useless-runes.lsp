(CLASS-NAMES-TO-PATHS
 (4 4 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE DEFAULT-CDR))
 )
(STRING-LISTP-OF-CLASS-NAMES-TO-PATHS
 (30 6 (:REWRITE DEFAULT-COERCE-1))
 (30 6 (:DEFINITION BINARY-APPEND))
 (24 6 (:REWRITE DEFAULT-COERCE-3))
 (15 15 (:REWRITE DEFAULT-CDR))
 (12 12 (:REWRITE DEFAULT-COERCE-2))
 (12 12 (:REWRITE DEFAULT-CAR))
 (6 6 (:REWRITE STRINGP-OF-TURN-DOTS-INTO-SLASHES))
 )
(EVENTS-FOR-CLASSES-FROM-ALIST
 (444 129 (:REWRITE JVM::CONSP-OF-CAR-WHEN-FIELD-INFO-ALISTP))
 (294 294 (:TYPE-PRESCRIPTION JVM::FIELD-INFO-ALISTP))
 (258 129 (:REWRITE CONSP-OF-CAR-WHEN-SYMBOL-TERM-ALISTP-CHEAP))
 (206 36 (:REWRITE UNSIGNED-BYTE-P-OF-CAR-WHEN-ALL-UNSIGNED-BYTE-P))
 (156 30 (:REWRITE ALL-UNSIGNED-BYTE-P-OF-CDR))
 (136 61 (:REWRITE ALL-UNSIGNED-BYTE-P-WHEN-NOT-CONSP))
 (129 129 (:TYPE-PRESCRIPTION SYMBOL-TERM-ALISTP))
 (117 39 (:REWRITE JVM::ALISTP-WHEN-METHOD-PROGRAMP))
 (101 33 (:REWRITE USE-ALL-UNSIGNED-BYTE-P-FOR-CAR))
 (78 78 (:TYPE-PRESCRIPTION JVM::METHOD-PROGRAMP))
 (63 21 (:REWRITE JVM::FIELD-INFO-ALISTP-OF-CDR))
 (56 38 (:REWRITE USE-ALL-UNSIGNED-BYTE-P))
 (39 39 (:REWRITE JVM::ALISTP-WHEN-JVM-INSTRUCTIONS-OKAYP))
 (38 38 (:REWRITE USE-ALL-UNSIGNED-BYTE-P-2))
 (31 31 (:REWRITE DEFAULT-<-2))
 (31 31 (:REWRITE DEFAULT-<-1))
 (18 18 (:TYPE-PRESCRIPTION MEMBER-EQUAL))
 (2 2 (:REWRITE LOOKUP-EQUAL-WHEN-NOT-CONSP-CHEAP))
 (2 2 (:REWRITE LOOKUP-EQUAL-WHEN-NOT-ASSOC-EQUAL-CHEAP))
 (1 1 (:REWRITE DEFAULT-+-2))
 (1 1 (:REWRITE DEFAULT-+-1))
 )
(TRUE-LISTP-OF-MV-NTH-1-OF-EVENTS-FOR-CLASSES-FROM-ALIST
 (145 54 (:REWRITE DEFAULT-CAR))
 (107 42 (:REWRITE DEFAULT-CDR))
 (96 30 (:REWRITE JVM::CONSP-OF-CAR-WHEN-FIELD-INFO-ALISTP))
 (62 62 (:TYPE-PRESCRIPTION JVM::FIELD-INFO-ALISTP))
 (60 30 (:REWRITE CONSP-OF-CAR-WHEN-SYMBOL-TERM-ALISTP-CHEAP))
 (51 7 (:DEFINITION REVAPPEND))
 (30 30 (:TYPE-PRESCRIPTION SYMBOL-TERM-ALISTP))
 (16 9 (:REWRITE DEFAULT-+-2))
 (14 14 (:REWRITE LOOKUP-EQUAL-WHEN-NOT-CONSP-CHEAP))
 (14 14 (:REWRITE LOOKUP-EQUAL-WHEN-NOT-ASSOC-EQUAL-CHEAP))
 (12 4 (:REWRITE JVM::FIELD-INFO-ALISTP-OF-CDR))
 (9 9 (:REWRITE DEFAULT-+-1))
 (8 2 (:REWRITE FOLD-CONSTS-IN-+))
 (7 7 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (2 2 (:REWRITE CDR-CONS))
 (2 2 (:REWRITE CAR-CONS))
 )
(READ-JAR-FN
 (92 4 (:DEFINITION EVENTS-FOR-CLASSES-FROM-ALIST))
 (72 32 (:REWRITE DEFAULT-CDR))
 (70 30 (:REWRITE DEFAULT-CAR))
 (48 16 (:REWRITE JVM::CONSP-OF-CAR-WHEN-FIELD-INFO-ALISTP))
 (42 6 (:REWRITE DEFAULT-COERCE-3))
 (36 12 (:REWRITE STATE-P-IMPLIES-AND-FORWARD-TO-STATE-P1))
 (36 4 (:DEFINITION STRIP-CDRS))
 (36 4 (:DEFINITION STRIP-CARS))
 (32 32 (:TYPE-PRESCRIPTION JVM::FIELD-INFO-ALISTP))
 (32 16 (:REWRITE CONSP-OF-CAR-WHEN-SYMBOL-TERM-ALISTP-CHEAP))
 (20 20 (:REWRITE DEFAULT-COERCE-2))
 (18 14 (:REWRITE DEFAULT-COERCE-1))
 (16 16 (:TYPE-PRESCRIPTION SYMBOL-TERM-ALISTP))
 (16 4 (:DEFINITION PARSE-CLASS-FILE-BYTES))
 (12 4 (:REWRITE LOOKUP-EQ-BECOMES-LOOKUP-EQUAL))
 (4 4 (:TYPE-PRESCRIPTION STRING-STARTS-WITHP))
 (4 4 (:TYPE-PRESCRIPTION STRING-ENDS-WITHP))
 (4 4 (:REWRITE LOOKUP-EQUAL-WHEN-NOT-CONSP-CHEAP))
 (4 4 (:REWRITE LOOKUP-EQUAL-WHEN-NOT-ASSOC-EQUAL-CHEAP))
 (4 4 (:REWRITE EQUAL-CONSTANT-WHEN-BVCHOP-EQUAL-CONSTANT-FALSE))
 (4 4 (:REWRITE DEFAULT-SYMBOL-PACKAGE-NAME))
 )