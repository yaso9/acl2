(JVM::LOAD_CLASS_DOES_NOT_CHANGE_ENV)
(JVM::SUPERCLASS1-MEASURE)
(JVM::ISSUPERCLASS1-INVARIANT)
(JVM::ISSUPERCLASS1
 (3938 1 (:REWRITE JVM::LOADER-INV-CLASS-LOADED-IMPLIES-ISSUPERCLASS-INVARIANT))
 (3936 1 (:DEFINITION JVM::LOADER-INV))
 (3198 1 (:DEFINITION JVM::WFF-STATIC-CLASS-TABLE-STRONG))
 (3179 1 (:DEFINITION JVM::WFF-CLASS-REP-STATIC-STRONG))
 (1824 130 (:DEFINITION MEM))
 (1641 11 (:DEFINITION JVM::PRIMITIVE-TYPE?))
 (1398 2 (:DEFINITION JVM::WFF-STATIC-CP-OK-S))
 (1348 2 (:DEFINITION JVM::WFF-STATIC-CP-ENTRY-OK-S))
 (1276 2 (:DEFINITION JVM::RUNTIME-METHOD-REP-GUARDS))
 (1246 2 (:DEFINITION JVM::RUNTIME-METHOD-REP-GUARD))
 (1088 418 (:DEFINITION NTH))
 (906 5 (:DEFINITION JVM::WFF-TYPE-REP))
 (807 796 (:REWRITE DEFAULT-CDR))
 (664 2 (:DEFINITION JVM::VALUE-TYPE-OK))
 (659 651 (:REWRITE DEFAULT-CAR))
 (404 2 (:DEFINITION JVM::WFF-TYPE-REPS))
 (343 1 (:DEFINITION JVM::LOADER-INV-HELPER))
 (333 1 (:DEFINITION JVM::LOADER-INV-HELPER1))
 (325 1 (:DEFINITION JVM::WFF-INSTANCE-CLASS-TABLE-STRONG))
 (315 61 (:DEFINITION LEN))
 (314 1 (:DEFINITION JVM::WFF-CLASS-REP-STRONG))
 (288 2 (:DEFINITION JVM::VALUE-TYPE-OK-2))
 (284 246 (:REWRITE MEM-SUBSET))
 (276 2 (:DEFINITION JVM::ALL-CORRECTLY-LOADED?))
 (242 24 (:DEFINITION JVM::ARRAY-TYPE?))
 (200 1 (:DEFINITION JVM::WFF-FIELDS-S))
 (176 2 (:DEFINITION JVM::NORMALIZE-TYPE-REP))
 (158 97 (:REWRITE DEFAULT-+-2))
 (154 35 (:DEFINITION TRUE-LISTP))
 (152 4 (:DEFINITION JVM::WFF-CODE-S))
 (144 36 (:REWRITE ZP-OPEN))
 (128 5 (:DEFINITION SET-DIFF))
 (128 2 (:DEFINITION JVM::WFF-CONSTANT-POOL-S))
 (118 2 (:DEFINITION JVM::CORRECTLY-LOADED?))
 (114 2 (:DEFINITION JVM::WFF-CONSTANT-POOL-ENTRY-S))
 (112 112 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (112 112 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (110 2 (:DEFINITION JVM::WFF-METHOD-DECL-S))
 (99 1 (:DEFINITION JVM::WFF-CLASS-REP))
 (97 97 (:REWRITE DEFAULT-+-1))
 (81 78 (:REWRITE DEFAULT-<-2))
 (80 78 (:REWRITE DEFAULT-<-1))
 (75 3 (:DEFINITION JVM::WFF-FIELD-S))
 (66 17 (:DEFINITION MV-NTH))
 (65 13 (:DEFINITION INT32P))
 (64 2 (:DEFINITION JVM::WFF-FIELDS-S-X))
 (64 1 (:DEFINITION JVM::WFF-CONSTANT-POOL))
 (62 1 (:DEFINITION JVM::WFF-CLASS-METHOD-DECLS))
 (61 61 (:REWRITE DEL-SET-LEN))
 (57 19 (:DEFINITION JVM::FIELD-FIELDTYPE-S))
 (57 1 (:DEFINITION JVM::WFF-CONSTANT-POOL-ENTRY))
 (55 55 (:TYPE-PRESCRIPTION JVM::CLASS-BY-NAME-S))
 (55 1 (:DEFINITION JVM::WFF-METHOD-DECL))
 (53 1 (:DEFINITION JVM::COLLECT-ASSIGNABLETONAME))
 (52 2 (:DEFINITION JVM::COLLECT-INTERFACE-X-ENV))
 (50 2 (:DEFINITION JVM::REFERENCE-TYPE))
 (49 2 (:DEFINITION JVM::CLASS-IS-LOADED?))
 (48 12 (:DEFINITION JVM::CLASS-BY-NAME-S))
 (48 8 (:REWRITE SET-EQUAL-MEM-CONS-2))
 (48 8 (:DEFINITION JVM::ALL-CLASS-NAMES-S))
 (37 1 (:DEFINITION JVM::WFF-INTERFACE-CLASS-STATIC))
 (36 12 (:DEFINITION JVM::METHOD-CODE-S))
 (36 12 (:DEFINITION JVM::FIELD-CPINDEX-S))
 (35 2 (:DEFINITION SUBSET))
 (35 1 (:DEFINITION JVM::ALL-FIELDS-STATIC-FINAL))
 (34 2 (:DEFINITION JVM::ISCLASSTERM))
 (33 1 (:DEFINITION JVM::WFF-CLASS-TABLE))
 (32 8 (:DEFINITION JVM::METHOD-ACCESSFLAGS-S))
 (32 2 (:DEFINITION JVM::COLLECT-SUPERCLASSNAME1))
 (32 1 (:DEFINITION JVM::WFF-STATIC-FIELDS-X))
 (32 1 (:DEFINITION JVM::WFF-CLASS-FIELDS))
 (30 6 (:DEFINITION APP))
 (30 2 (:DEFINITION JVM::WFF-LONG-CP-ENTRY-S))
 (30 2 (:DEFINITION JVM::WFF-INT-CP-ENTRY-S))
 (29 1 (:DEFINITION JVM::COLLECT-SUPERINTERFACE))
 (28 1 (:DEFINITION JVM::COLLECT-SUPERINTERFACE1))
 (27 27 (:TYPE-PRESCRIPTION JVM::WFF-TYPE-REP))
 (26 4 (:DEFINITION JVM::CLASS-BY-NAME))
 (25 5 (:DEFINITION INT64P))
 (25 1 (:DEFINITION JVM::WFF-STATIC-FIELD))
 (25 1 (:DEFINITION JVM::WFF-FIELD))
 (25 1 (:DEFINITION JVM::WFF-ENV))
 (23 23 (:TYPE-PRESCRIPTION JVM::ALL-CLASS-NAMES-S))
 (22 2 (:DEFINITION JVM::CLASS-IS-LOADED-FROM))
 (22 1 (:DEFINITION NODUP-SET))
 (21 7 (:DEFINITION JVM::CLASSNAME))
 (21 7 (:DEFINITION JVM::ARRAY-BASE-TYPE))
 (20 2 (:DEFINITION JVM::WFF-STRING-CP-ENTRY-S))
 (20 2 (:DEFINITION JVM::STATIC-FIELD?-S))
 (18 1 (:DEFINITION JVM::COLLECT-SUPERCLASSNAME))
 (16 4 (:DEFINITION JVM::METHOD-RETURNTYPE-S))
 (16 4 (:DEFINITION JVM::FIELD-FIELDACCESSFLAGS-S))
 (16 4 (:DEFINITION JVM::CONSTANTPOOL-S))
 (16 4 (:DEFINITION JVM::CODE-MAX-STACK-S))
 (16 4 (:DEFINITION JVM::CODE-MAX-LOCAL-S))
 (16 2 (:REWRITE JVM::FOUND-IMPLIES-MEM))
 (15 1 (:DEFINITION JVM::WFF-LONG-CP-ENTRY))
 (15 1 (:DEFINITION JVM::WFF-INT-CP-ENTRY))
 (14 14 (:TYPE-PRESCRIPTION JVM::WFF-STATIC-CP-OK-S))
 (12 12 (:TYPE-PRESCRIPTION JVM::WFF-TYPE-REPS))
 (12 12 (:TYPE-PRESCRIPTION JVM::WFF-FIELDS-S-X))
 (12 12 (:TYPE-PRESCRIPTION JVM::WFF-CONSTANT-POOL-S))
 (12 12 (:TYPE-PRESCRIPTION JVM::RUNTIME-METHOD-REP-GUARDS))
 (12 6 (:TYPE-PRESCRIPTION APP))
 (12 2 (:DEFINITION SHORTP))
 (12 2 (:DEFINITION JVMBOOLEANP))
 (12 2 (:DEFINITION CHARP))
 (12 2 (:DEFINITION BYTEP))
 (10 10 (:TYPE-PRESCRIPTION SUBSET))
 (10 10 (:TYPE-PRESCRIPTION SET-DIFF))
 (10 10 (:TYPE-PRESCRIPTION JVM::COLLECT-INTERFACE-X-ENV))
 (10 1 (:DEFINITION JVM::WFF-STRING-CP-ENTRY))
 (8 8 (:REWRITE SET-EQUAL-CONS))
 (8 2 (:DEFINITION JVM::METHOD-ARGS-S))
 (8 2 (:DEFINITION JVM::INTERFACES))
 (7 7 (:TYPE-PRESCRIPTION JVM::ALL-CORRECTLY-LOADED?))
 (7 7 (:REWRITE CDR-CONS))
 (7 7 (:REWRITE CAR-CONS))
 (6 6 (:TYPE-PRESCRIPTION JVM::WFF-STATIC-FIELDS-X))
 (6 6 (:TYPE-PRESCRIPTION JVM::WFF-FIELDS-S))
 (6 6 (:TYPE-PRESCRIPTION JVM::WFF-CLASS-METHOD-DECLS))
 (6 6 (:TYPE-PRESCRIPTION JVM::WFF-CLASS-FIELDS))
 (6 6 (:TYPE-PRESCRIPTION NODUP-SET))
 (6 6 (:TYPE-PRESCRIPTION JVM::ALL-FIELDS-STATIC-FINAL))
 (6 2 (:DEFINITION JVM::SUPER))
 (6 2 (:DEFINITION JVM::CPENTRY-VALUE-S))
 (6 1 (:DEFINITION JVM::COLLECT-STATIC-FIELD-NAMES))
 (5 5 (:TYPE-PRESCRIPTION JVM::WFF-STATIC-CLASS-TABLE-STRONG))
 (5 5 (:TYPE-PRESCRIPTION JVM::WFF-INSTANCE-CLASS-TABLE-STRONG))
 (5 5 (:TYPE-PRESCRIPTION JVM::WFF-CONSTANT-POOL))
 (5 5 (:TYPE-PRESCRIPTION JVM::COLLECT-STATIC-FIELD-NAMES))
 (4 4 (:TYPE-PRESCRIPTION JVM::LOADER-INV-HELPER))
 (4 4 (:TYPE-PRESCRIPTION JVM::COLLECT-SUPERCLASSNAME1))
 (4 4 (:REWRITE SUBSET-TRANSITIVE))
 (4 2 (:DEFINITION JVM::CPENTRY-TYPE-S))
 (4 1 (:DEFINITION JVM::STATIC-FIELDS))
 (4 1 (:DEFINITION JVM::METHODS))
 (4 1 (:DEFINITION JVM::FIELDS))
 (4 1 (:DEFINITION JVM::CONSTANTPOOL))
 (4 1 (:DEFINITION JVM::ACCESSFLAGS-S))
 (3 3 (:TYPE-PRESCRIPTION JVM::WFF-CLASS-REP-STATIC))
 (3 1 (:REWRITE JVM::NO-FATAL-ERROR?-AFTER-LOAD-IMPLIES-CLASS-EXISTS-EXTERNALLY))
 (3 1 (:DEFINITION JVM::FIELD-FIELDNAME-S))
 (2 2 (:TYPE-PRESCRIPTION JVM::WFF-STATE))
 (2 2 (:DEFINITION JVM::MAKE-ARRAY-TYPE))
 (2 2 (:DEFINITION JVMFLOATP))
 (2 2 (:DEFINITION DOUBLEP))
 (1 1 (:TYPE-PRESCRIPTION JVM::LOADER-INV))
 (1 1 (:REWRITE JVM::NO-FATAL-ERROR?-AFTER-LOAD-CLASS-X-IMPLIES-CLASS-EXISTS-EXTERNALLY))
 (1 1 (:REWRITE LEN-SET-EQUAL-NODUP-SET-X))
 (1 1 (:DEFINITION JVM::EXTERNAL-CLASS-TABLE))
 )
(JVM::ISSUPERCLASS)
(JVM::S-NOT-CHANGED-ISSUPERCLASS1
 (91269 21 (:REWRITE JVM::LOADER-INV-CLASS-LOADED-IMPLIES-ISSUPERCLASS-INVARIANT))
 (91227 21 (:DEFINITION JVM::LOADER-INV))
 (76752 24 (:DEFINITION JVM::WFF-STATIC-CLASS-TABLE-STRONG))
 (76296 24 (:DEFINITION JVM::WFF-CLASS-REP-STATIC-STRONG))
 (39384 264 (:DEFINITION JVM::PRIMITIVE-TYPE?))
 (33552 48 (:DEFINITION JVM::WFF-STATIC-CP-OK-S))
 (32352 48 (:DEFINITION JVM::WFF-STATIC-CP-ENTRY-OK-S))
 (30624 48 (:DEFINITION JVM::RUNTIME-METHOD-REP-GUARDS))
 (29904 48 (:DEFINITION JVM::RUNTIME-METHOD-REP-GUARD))
 (25698 9825 (:DEFINITION NTH))
 (21744 120 (:DEFINITION JVM::WFF-TYPE-REP))
 (17911 17709 (:REWRITE DEFAULT-CDR))
 (15936 48 (:DEFINITION JVM::VALUE-TYPE-OK))
 (14468 14324 (:REWRITE DEFAULT-CAR))
 (9696 48 (:DEFINITION JVM::WFF-TYPE-REPS))
 (6912 48 (:DEFINITION JVM::VALUE-TYPE-OK-2))
 (6825 21 (:DEFINITION JVM::WFF-INSTANCE-CLASS-TABLE-STRONG))
 (6822 6822 (:TYPE-PRESCRIPTION LEN))
 (6594 21 (:DEFINITION JVM::WFF-CLASS-REP-STRONG))
 (6480 1296 (:DEFINITION LEN))
 (6174 18 (:DEFINITION JVM::LOADER-INV-HELPER))
 (5994 18 (:DEFINITION JVM::LOADER-INV-HELPER1))
 (5808 576 (:DEFINITION JVM::ARRAY-TYPE?))
 (4968 36 (:DEFINITION JVM::ALL-CORRECTLY-LOADED?))
 (4800 24 (:DEFINITION JVM::WFF-FIELDS-S))
 (4224 48 (:DEFINITION JVM::NORMALIZE-TYPE-REP))
 (3648 96 (:DEFINITION JVM::WFF-CODE-S))
 (3471 795 (:DEFINITION TRUE-LISTP))
 (3456 2160 (:REWRITE DEFAULT-+-2))
 (3456 864 (:REWRITE ZP-OPEN))
 (3072 48 (:DEFINITION JVM::WFF-CONSTANT-POOL-S))
 (2736 48 (:DEFINITION JVM::WFF-CONSTANT-POOL-ENTRY-S))
 (2640 48 (:DEFINITION JVM::WFF-METHOD-DECL-S))
 (2532 2532 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (2508 2508 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (2160 2160 (:REWRITE DEFAULT-+-1))
 (2124 36 (:DEFINITION JVM::CORRECTLY-LOADED?))
 (2079 21 (:DEFINITION JVM::WFF-CLASS-REP))
 (1860 1812 (:REWRITE DEFAULT-<-2))
 (1812 1812 (:REWRITE DEFAULT-<-1))
 (1800 72 (:DEFINITION JVM::WFF-FIELD-S))
 (1545 309 (:DEFINITION INT32P))
 (1536 48 (:DEFINITION JVM::WFF-FIELDS-S-X))
 (1368 456 (:DEFINITION JVM::FIELD-FIELDTYPE-S))
 (1344 21 (:DEFINITION JVM::WFF-CONSTANT-POOL))
 (1302 21 (:DEFINITION JVM::WFF-CLASS-METHOD-DECLS))
 (1296 1296 (:REWRITE DEL-SET-LEN))
 (1200 48 (:DEFINITION JVM::REFERENCE-TYPE))
 (1197 21 (:DEFINITION JVM::WFF-CONSTANT-POOL-ENTRY))
 (1155 21 (:DEFINITION JVM::WFF-METHOD-DECL))
 (986 986 (:TYPE-PRESCRIPTION JVM::CLASS-BY-NAME-S))
 (954 18 (:DEFINITION JVM::COLLECT-ASSIGNABLETONAME))
 (936 36 (:DEFINITION JVM::COLLECT-INTERFACE-X-ENV))
 (888 24 (:DEFINITION JVM::WFF-INTERFACE-CLASS-STATIC))
 (882 36 (:DEFINITION JVM::CLASS-IS-LOADED?))
 (864 288 (:DEFINITION JVM::METHOD-CODE-S))
 (864 288 (:DEFINITION JVM::FIELD-CPINDEX-S))
 (856 214 (:DEFINITION JVM::CLASS-BY-NAME-S))
 (840 24 (:DEFINITION JVM::ALL-FIELDS-STATIC-FINAL))
 (768 192 (:DEFINITION JVM::METHOD-ACCESSFLAGS-S))
 (720 48 (:DEFINITION JVM::WFF-LONG-CP-ENTRY-S))
 (720 48 (:DEFINITION JVM::WFF-INT-CP-ENTRY-S))
 (693 21 (:DEFINITION JVM::WFF-CLASS-TABLE))
 (672 21 (:DEFINITION JVM::WFF-STATIC-FIELDS-X))
 (672 21 (:DEFINITION JVM::WFF-CLASS-FIELDS))
 (648 648 (:TYPE-PRESCRIPTION JVM::WFF-TYPE-REP))
 (612 36 (:DEFINITION JVM::ISCLASSTERM))
 (585 117 (:DEFINITION INT64P))
 (576 36 (:DEFINITION JVM::COLLECT-SUPERCLASSNAME1))
 (540 108 (:DEFINITION APP))
 (528 24 (:DEFINITION NODUP-SET))
 (525 21 (:DEFINITION JVM::WFF-STATIC-FIELD))
 (525 21 (:DEFINITION JVM::WFF-FIELD))
 (525 21 (:DEFINITION JVM::WFF-ENV))
 (522 18 (:DEFINITION JVM::COLLECT-SUPERINTERFACE))
 (510 78 (:DEFINITION JVM::CLASS-BY-NAME))
 (504 168 (:DEFINITION JVM::ARRAY-BASE-TYPE))
 (504 18 (:DEFINITION JVM::COLLECT-SUPERINTERFACE1))
 (480 48 (:DEFINITION JVM::WFF-STRING-CP-ENTRY-S))
 (480 48 (:DEFINITION JVM::STATIC-FIELD?-S))
 (432 16 (:DEFINITION JVM::ALL-CLASS-NAMES-S))
 (396 132 (:DEFINITION JVM::CLASSNAME))
 (396 36 (:DEFINITION JVM::CLASS-IS-LOADED-FROM))
 (384 96 (:DEFINITION JVM::METHOD-RETURNTYPE-S))
 (384 96 (:DEFINITION JVM::FIELD-FIELDACCESSFLAGS-S))
 (384 96 (:DEFINITION JVM::CONSTANTPOOL-S))
 (384 96 (:DEFINITION JVM::CODE-MAX-STACK-S))
 (384 96 (:DEFINITION JVM::CODE-MAX-LOCAL-S))
 (368 16 (:REWRITE SET-EQUAL-MEM-CONS-2))
 (349 19 (:DEFINITION SUBSET))
 (336 336 (:TYPE-PRESCRIPTION JVM::WFF-STATIC-CP-OK-S))
 (324 18 (:DEFINITION JVM::COLLECT-SUPERCLASSNAME))
 (315 21 (:DEFINITION JVM::WFF-LONG-CP-ENTRY))
 (315 21 (:DEFINITION JVM::WFF-INT-CP-ENTRY))
 (288 288 (:TYPE-PRESCRIPTION JVM::WFF-TYPE-REPS))
 (288 288 (:TYPE-PRESCRIPTION JVM::WFF-FIELDS-S-X))
 (288 288 (:TYPE-PRESCRIPTION JVM::WFF-CONSTANT-POOL-S))
 (288 288 (:TYPE-PRESCRIPTION JVM::RUNTIME-METHOD-REP-GUARDS))
 (288 48 (:DEFINITION SHORTP))
 (288 48 (:DEFINITION JVMBOOLEANP))
 (288 48 (:DEFINITION CHARP))
 (288 48 (:DEFINITION BYTEP))
 (272 34 (:REWRITE JVM::FOUND-IMPLIES-MEM))
 (216 108 (:TYPE-PRESCRIPTION APP))
 (210 21 (:DEFINITION JVM::WFF-STRING-CP-ENTRY))
 (192 48 (:DEFINITION JVM::METHOD-ARGS-S))
 (180 180 (:TYPE-PRESCRIPTION JVM::COLLECT-INTERFACE-X-ENV))
 (144 144 (:TYPE-PRESCRIPTION JVM::WFF-FIELDS-S))
 (144 144 (:TYPE-PRESCRIPTION NODUP-SET))
 (144 144 (:TYPE-PRESCRIPTION JVM::ALL-FIELDS-STATIC-FINAL))
 (144 48 (:DEFINITION JVM::CPENTRY-VALUE-S))
 (144 36 (:DEFINITION JVM::INTERFACES))
 (144 24 (:DEFINITION JVM::COLLECT-STATIC-FIELD-NAMES))
 (126 126 (:TYPE-PRESCRIPTION JVM::WFF-STATIC-FIELDS-X))
 (126 126 (:TYPE-PRESCRIPTION JVM::WFF-CLASS-METHOD-DECLS))
 (126 126 (:TYPE-PRESCRIPTION JVM::WFF-CLASS-FIELDS))
 (126 126 (:TYPE-PRESCRIPTION JVM::ALL-CORRECTLY-LOADED?))
 (120 120 (:TYPE-PRESCRIPTION JVM::WFF-STATIC-CLASS-TABLE-STRONG))
 (120 120 (:TYPE-PRESCRIPTION JVM::COLLECT-STATIC-FIELD-NAMES))
 (105 105 (:TYPE-PRESCRIPTION JVM::WFF-INSTANCE-CLASS-TABLE-STRONG))
 (105 105 (:TYPE-PRESCRIPTION JVM::WFF-CONSTANT-POOL))
 (96 48 (:DEFINITION JVM::CPENTRY-TYPE-S))
 (96 24 (:DEFINITION JVM::ACCESSFLAGS-S))
 (84 21 (:DEFINITION JVM::STATIC-FIELDS))
 (84 21 (:DEFINITION JVM::METHODS))
 (84 21 (:DEFINITION JVM::FIELDS))
 (84 21 (:DEFINITION JVM::CONSTANTPOOL))
 (72 72 (:TYPE-PRESCRIPTION JVM::WFF-CLASS-REP-STATIC))
 (72 72 (:TYPE-PRESCRIPTION JVM::LOADER-INV-HELPER))
 (72 72 (:TYPE-PRESCRIPTION JVM::COLLECT-SUPERCLASSNAME1))
 (72 24 (:DEFINITION JVM::FIELD-FIELDNAME-S))
 (55 55 (:REWRITE CAR-CONS))
 (48 48 (:DEFINITION JVM::MAKE-ARRAY-TYPE))
 (48 48 (:DEFINITION JVMFLOATP))
 (48 48 (:DEFINITION DOUBLEP))
 (46 46 (:REWRITE SUBSET-TRANSITIVE))
 (45 15 (:REWRITE JVM::NO-FATAL-ERROR?-AFTER-LOAD-IMPLIES-CLASS-EXISTS-EXTERNALLY))
 (42 42 (:TYPE-PRESCRIPTION JVM::WFF-STATE))
 (21 21 (:TYPE-PRESCRIPTION JVM::LOADER-INV))
 (21 21 (:DEFINITION JVM::EXTERNAL-CLASS-TABLE))
 (16 16 (:REWRITE SET-EQUAL-CONS))
 (15 15 (:REWRITE JVM::NO-FATAL-ERROR?-AFTER-LOAD-CLASS-X-IMPLIES-CLASS-EXISTS-EXTERNALLY))
 (8 8 (:TYPE-PRESCRIPTION JVM::ISSUPERCLASS1))
 )
(JVM::UNSEEN-CLASS-COUNT)
(JVM::CLASSIMPLEMENTS-MEASURE)
(JVM::CLASSIMPLEMENTSINTERFACE1-INVARIANT)
(JVM::CLASSIMPLEMENTSINTERFACE1-AUX-INVARIANT)
(JVM::INTERFACESIMPLEMENTSINTERFACE1-INV)
(JVM::SIMPLE-INV1)
(JVM::IMPLEMENTINTERFACE-X-MEASURE)
(JVM::ALL-LOADED?-X
 (74 36 (:REWRITE DEFAULT-+-2))
 (50 36 (:REWRITE DEFAULT-+-1))
 (36 4 (:DEFINITION LENGTH))
 (32 8 (:REWRITE COMMUTATIVITY-OF-+))
 (32 8 (:DEFINITION INTEGER-ABS))
 (20 4 (:DEFINITION LEN))
 (12 10 (:REWRITE DEFAULT-<-2))
 (12 10 (:REWRITE DEFAULT-<-1))
 (11 11 (:REWRITE DEFAULT-CDR))
 (10 10 (:REWRITE FOLD-CONSTS-IN-+))
 (8 8 (:REWRITE DEFAULT-UNARY-MINUS))
 (7 7 (:REWRITE DEFAULT-CAR))
 (4 4 (:TYPE-PRESCRIPTION LEN))
 (4 4 (:REWRITE DEL-SET-LEN))
 (4 4 (:REWRITE DEFAULT-REALPART))
 (4 4 (:REWRITE DEFAULT-NUMERATOR))
 (4 4 (:REWRITE DEFAULT-IMAGPART))
 (4 4 (:REWRITE DEFAULT-DENOMINATOR))
 (4 4 (:REWRITE DEFAULT-COERCE-2))
 (4 4 (:REWRITE DEFAULT-COERCE-1))
 )
(JVM::IMPLEMENTINTERFACE-X-GUARD)
(JVM::IMPLEMENTINTERFACE-X
 (499 485 (:REWRITE DEFAULT-CAR))
 (462 454 (:REWRITE DEFAULT-CDR))
 (386 196 (:REWRITE MEM-SUBSET))
 (312 72 (:DEFINITION JVM::ALL-CLASS-NAMES-S))
 (274 137 (:REWRITE DEFAULT-+-2))
 (244 66 (:REWRITE SET-EQUAL-MEM-CONS-2))
 (175 10 (:DEFINITION SUBSET))
 (167 167 (:TYPE-PRESCRIPTION JVM::ALL-CLASS-NAMES-S))
 (137 137 (:REWRITE DEFAULT-+-1))
 (108 108 (:TYPE-PRESCRIPTION SET-DIFF))
 (105 15 (:DEFINITION JVM::CLASS-BY-NAME))
 (71 71 (:REWRITE DEL-SET-LEN))
 (66 66 (:REWRITE SET-EQUAL-CONS))
 (64 8 (:REWRITE JVM::FOUND-IMPLIES-MEM))
 (50 50 (:TYPE-PRESCRIPTION SUBSET))
 (45 15 (:DEFINITION JVM::CLASSNAME))
 (32 8 (:DEFINITION JVM::CLASS-BY-NAME-S))
 (30 15 (:DEFINITION NTH))
 (29 18 (:REWRITE DEFAULT-<-1))
 (27 18 (:REWRITE DEFAULT-<-2))
 (20 20 (:REWRITE SUBSET-TRANSITIVE))
 (16 16 (:TYPE-PRESCRIPTION JVM::CLASS-BY-NAME-S))
 (8 8 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (3 1 (:REWRITE JVM::NO-FATAL-ERROR?-AFTER-LOAD-IMPLIES-CLASS-EXISTS-EXTERNALLY))
 (1 1 (:REWRITE JVM::NO-FATAL-ERROR?-AFTER-LOAD-CLASS-X-IMPLIES-CLASS-EXISTS-EXTERNALLY))
 )
(JVM::INTERFACESIMPLEMENTSINTERFACE)
(JVM::CLASSIMPLEMENTSINTERFACE)
(JVM::STATE-NO-CHANGE-LEMMA
 (18707 785 (:DEFINITION SUBSET))
 (9942 658 (:DEFINITION JVM::ALL-CLASS-NAMES-S))
 (7636 332 (:REWRITE SET-EQUAL-MEM-CONS-2))
 (6941 6578 (:REWRITE DEFAULT-CAR))
 (6555 5908 (:REWRITE DEFAULT-CDR))
 (5624 703 (:REWRITE JVM::FOUND-IMPLIES-MEM))
 (4249 607 (:DEFINITION JVM::CLASS-BY-NAME))
 (2812 703 (:DEFINITION JVM::CLASS-BY-NAME-S))
 (2008 2008 (:REWRITE SUBSET-TRANSITIVE))
 (1821 607 (:DEFINITION JVM::CLASSNAME))
 (1406 1406 (:TYPE-PRESCRIPTION JVM::CLASS-BY-NAME-S))
 (1214 607 (:DEFINITION NTH))
 (942 314 (:REWRITE JVM::NO-FATAL-ERROR?-AFTER-LOAD-IMPLIES-CLASS-EXISTS-EXTERNALLY))
 (332 332 (:REWRITE SET-EQUAL-CONS))
 (314 314 (:REWRITE JVM::NO-FATAL-ERROR?-AFTER-LOAD-CLASS-X-IMPLIES-CLASS-EXISTS-EXTERNALLY))
 )
(JVM::ISASSIGNABLETO
 (177 177 (:REWRITE DEFAULT-CDR))
 (152 75 (:REWRITE DEFAULT-+-2))
 (95 75 (:REWRITE DEFAULT-+-1))
 (54 6 (:DEFINITION LENGTH))
 (51 51 (:REWRITE MEM-SUBSET))
 (48 12 (:REWRITE COMMUTATIVITY-OF-+))
 (48 12 (:DEFINITION INTEGER-ABS))
 (30 30 (:REWRITE DEFAULT-CAR))
 (22 17 (:REWRITE DEFAULT-<-1))
 (21 17 (:REWRITE DEFAULT-<-2))
 (20 20 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (16 16 (:REWRITE FOLD-CONSTS-IN-+))
 (15 15 (:REWRITE DEL-SET-LEN))
 (14 7 (:DEFINITION TRUE-LISTP))
 (12 12 (:REWRITE DEFAULT-UNARY-MINUS))
 (6 6 (:REWRITE DEFAULT-REALPART))
 (6 6 (:REWRITE DEFAULT-NUMERATOR))
 (6 6 (:REWRITE DEFAULT-IMAGPART))
 (6 6 (:REWRITE DEFAULT-DENOMINATOR))
 (6 6 (:REWRITE DEFAULT-COERCE-2))
 (6 6 (:REWRITE DEFAULT-COERCE-1))
 )
(JVM::STATE-NO-CHANGE-ISASSIGNABLE
 (47796 12 (:DEFINITION JVM::ISSUPERCLASS1))
 (47040 12 (:REWRITE JVM::LOADER-INV-CLASS-LOADED-IMPLIES-ISSUPERCLASS-INVARIANT))
 (47016 12 (:DEFINITION JVM::LOADER-INV))
 (38376 12 (:DEFINITION JVM::WFF-STATIC-CLASS-TABLE-STRONG))
 (38148 12 (:DEFINITION JVM::WFF-CLASS-REP-STATIC-STRONG))
 (16776 24 (:DEFINITION JVM::WFF-STATIC-CP-OK-S))
 (16176 24 (:DEFINITION JVM::WFF-STATIC-CP-ENTRY-OK-S))
 (15312 24 (:DEFINITION JVM::RUNTIME-METHOD-REP-GUARDS))
 (14952 24 (:DEFINITION JVM::RUNTIME-METHOD-REP-GUARD))
 (10872 60 (:DEFINITION JVM::WFF-TYPE-REP))
 (10062 9926 (:REWRITE DEFAULT-CDR))
 (7968 24 (:DEFINITION JVM::VALUE-TYPE-OK))
 (7834 7738 (:REWRITE DEFAULT-CAR))
 (4848 24 (:DEFINITION JVM::WFF-TYPE-REPS))
 (4318 4318 (:REWRITE MEM-SUBSET))
 (3900 12 (:DEFINITION JVM::WFF-INSTANCE-CLASS-TABLE-STRONG))
 (3888 12 (:DEFINITION JVM::LOADER-INV-HELPER))
 (3768 12 (:DEFINITION JVM::WFF-CLASS-REP-STRONG))
 (3768 12 (:DEFINITION JVM::LOADER-INV-HELPER1))
 (3456 24 (:DEFINITION JVM::VALUE-TYPE-OK-2))
 (3084 24 (:DEFINITION JVM::ALL-CORRECTLY-LOADED?))
 (2400 12 (:DEFINITION JVM::WFF-FIELDS-S))
 (2112 24 (:DEFINITION JVM::NORMALIZE-TYPE-REP))
 (2030 1231 (:REWRITE DEFAULT-+-2))
 (2012 502 (:DEFINITION TRUE-LISTP))
 (1824 48 (:DEFINITION JVM::WFF-CODE-S))
 (1728 432 (:REWRITE ZP-OPEN))
 (1536 24 (:DEFINITION JVM::WFF-CONSTANT-POOL-S))
 (1368 24 (:DEFINITION JVM::WFF-CONSTANT-POOL-ENTRY-S))
 (1362 1362 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (1320 24 (:DEFINITION JVM::WFF-METHOD-DECL-S))
 (1248 24 (:DEFINITION JVM::CORRECTLY-LOADED?))
 (1231 1231 (:REWRITE DEFAULT-+-1))
 (1188 12 (:DEFINITION JVM::WFF-CLASS-REP))
 (1032 24 (:DEFINITION JVM::CLASS-EXISTS-EXTERNALLY?))
 (1026 150 (:DEFINITION JVM::CLASS-BY-NAME))
 (936 912 (:REWRITE DEFAULT-<-2))
 (912 912 (:REWRITE DEFAULT-<-1))
 (900 36 (:DEFINITION JVM::WFF-FIELD-S))
 (799 799 (:REWRITE DEL-SET-LEN))
 (780 156 (:DEFINITION INT32P))
 (768 24 (:DEFINITION JVM::WFF-FIELDS-S-X))
 (768 12 (:DEFINITION JVM::WFF-CONSTANT-POOL))
 (744 12 (:DEFINITION JVM::WFF-CLASS-METHOD-DECLS))
 (708 708 (:TYPE-PRESCRIPTION JVM::CLASS-BY-NAME-S))
 (684 228 (:DEFINITION JVM::FIELD-FIELDTYPE-S))
 (684 12 (:DEFINITION JVM::WFF-CONSTANT-POOL-ENTRY))
 (684 12 (:DEFINITION JVM::IMPLEMENTINTERFACE-X))
 (672 168 (:DEFINITION JVM::CLASS-BY-NAME-S))
 (660 12 (:DEFINITION JVM::WFF-METHOD-DECL))
 (648 24 (:DEFINITION JVM::ALL-CLASS-NAMES-S))
 (636 12 (:DEFINITION JVM::COLLECT-ASSIGNABLETONAME))
 (624 24 (:DEFINITION JVM::COLLECT-INTERFACE-X-ENV))
 (600 24 (:DEFINITION JVM::REFERENCE-TYPE))
 (588 24 (:DEFINITION JVM::CLASS-IS-LOADED?))
 (588 12 (:DEFINITION JVM::ISSUPERCLASS1-INVARIANT))
 (588 12 (:DEFINITION JVM::CLASSIMPLEMENTSINTERFACE1-INVARIANT))
 (558 186 (:DEFINITION JVM::CLASSNAME))
 (552 24 (:REWRITE SET-EQUAL-MEM-CONS-2))
 (444 12 (:DEFINITION JVM::WFF-INTERFACE-CLASS-STATIC))
 (432 144 (:DEFINITION JVM::METHOD-CODE-S))
 (432 144 (:DEFINITION JVM::FIELD-CPINDEX-S))
 (420 12 (:DEFINITION JVM::ALL-FIELDS-STATIC-FINAL))
 (408 24 (:DEFINITION JVM::ISCLASSTERM))
 (396 12 (:DEFINITION JVM::WFF-CLASS-TABLE))
 (384 96 (:DEFINITION JVM::METHOD-ACCESSFLAGS-S))
 (384 48 (:REWRITE JVM::FOUND-IMPLIES-MEM))
 (384 24 (:DEFINITION JVM::COLLECT-SUPERCLASSNAME1))
 (384 12 (:DEFINITION JVM::WFF-STATIC-FIELDS-X))
 (384 12 (:DEFINITION JVM::WFF-CLASS-FIELDS))
 (360 72 (:DEFINITION APP))
 (360 24 (:DEFINITION JVM::WFF-LONG-CP-ENTRY-S))
 (360 24 (:DEFINITION JVM::WFF-INT-CP-ENTRY-S))
 (348 12 (:DEFINITION JVM::COLLECT-SUPERINTERFACE))
 (336 12 (:DEFINITION JVM::COLLECT-SUPERINTERFACE1))
 (324 324 (:TYPE-PRESCRIPTION JVM::WFF-TYPE-REP))
 (300 60 (:DEFINITION INT64P))
 (300 12 (:DEFINITION JVM::WFF-STATIC-FIELD))
 (300 12 (:DEFINITION JVM::WFF-FIELD))
 (300 12 (:DEFINITION JVM::WFF-ENV))
 (264 12 (:DEFINITION NODUP-SET))
 (240 24 (:DEFINITION JVM::WFF-STRING-CP-ENTRY-S))
 (240 24 (:DEFINITION JVM::STATIC-FIELD?-S))
 (216 12 (:DEFINITION JVM::COLLECT-SUPERCLASSNAME))
 (192 48 (:DEFINITION JVM::METHOD-RETURNTYPE-S))
 (192 48 (:DEFINITION JVM::FIELD-FIELDACCESSFLAGS-S))
 (192 48 (:DEFINITION JVM::CONSTANTPOOL-S))
 (192 48 (:DEFINITION JVM::CODE-MAX-STACK-S))
 (192 48 (:DEFINITION JVM::CODE-MAX-LOCAL-S))
 (180 12 (:DEFINITION JVM::WFF-LONG-CP-ENTRY))
 (180 12 (:DEFINITION JVM::WFF-INT-CP-ENTRY))
 (168 168 (:TYPE-PRESCRIPTION JVM::WFF-STATIC-CP-OK-S))
 (144 144 (:TYPE-PRESCRIPTION JVM::WFF-TYPE-REPS))
 (144 144 (:TYPE-PRESCRIPTION JVM::WFF-FIELDS-S-X))
 (144 144 (:TYPE-PRESCRIPTION JVM::WFF-CONSTANT-POOL-S))
 (144 144 (:TYPE-PRESCRIPTION JVM::RUNTIME-METHOD-REP-GUARDS))
 (144 144 (:TYPE-PRESCRIPTION JVM::ALL-CLASS-NAMES-S))
 (144 72 (:TYPE-PRESCRIPTION APP))
 (144 24 (:DEFINITION SHORTP))
 (144 24 (:DEFINITION JVMBOOLEANP))
 (144 24 (:DEFINITION CHARP))
 (144 24 (:DEFINITION BYTEP))
 (120 120 (:TYPE-PRESCRIPTION JVM::COLLECT-INTERFACE-X-ENV))
 (120 12 (:DEFINITION JVM::WFF-STRING-CP-ENTRY))
 (96 96 (:TYPE-PRESCRIPTION JVM::NO-FATAL-ERROR?))
 (96 24 (:DEFINITION JVM::METHOD-ARGS-S))
 (96 24 (:DEFINITION JVM::CLASS-IS-LOADED-FROM))
 (84 84 (:TYPE-PRESCRIPTION JVM::CLASS-LOADED?))
 (84 84 (:TYPE-PRESCRIPTION JVM::ALL-CORRECTLY-LOADED?))
 (72 72 (:TYPE-PRESCRIPTION JVM::WFF-STATIC-FIELDS-X))
 (72 72 (:TYPE-PRESCRIPTION JVM::WFF-FIELDS-S))
 (72 72 (:TYPE-PRESCRIPTION JVM::WFF-CLASS-METHOD-DECLS))
 (72 72 (:TYPE-PRESCRIPTION JVM::WFF-CLASS-FIELDS))
 (72 72 (:TYPE-PRESCRIPTION NODUP-SET))
 (72 72 (:TYPE-PRESCRIPTION JVM::ALL-FIELDS-STATIC-FINAL))
 (72 24 (:REWRITE JVM::NO-FATAL-ERROR?-AFTER-LOAD-IMPLIES-CLASS-EXISTS-EXTERNALLY))
 (72 24 (:DEFINITION JVM::CPENTRY-VALUE-S))
 (72 12 (:DEFINITION JVM::COLLECT-STATIC-FIELD-NAMES))
 (60 60 (:TYPE-PRESCRIPTION JVM::WFF-STATIC-CLASS-TABLE-STRONG))
 (60 60 (:TYPE-PRESCRIPTION JVM::WFF-INSTANCE-CLASS-TABLE-STRONG))
 (60 60 (:TYPE-PRESCRIPTION JVM::WFF-CONSTANT-POOL))
 (60 60 (:TYPE-PRESCRIPTION JVM::COLLECT-STATIC-FIELD-NAMES))
 (48 48 (:TYPE-PRESCRIPTION JVM::LOADER-INV-HELPER))
 (48 48 (:TYPE-PRESCRIPTION JVM::COLLECT-SUPERCLASSNAME1))
 (48 24 (:DEFINITION JVM::CPENTRY-TYPE-S))
 (48 12 (:DEFINITION JVM::STATIC-FIELDS))
 (48 12 (:DEFINITION JVM::METHODS))
 (48 12 (:DEFINITION JVM::FIELDS))
 (48 12 (:DEFINITION JVM::CONSTANTPOOL))
 (48 12 (:DEFINITION JVM::ACCESSFLAGS-S))
 (36 36 (:TYPE-PRESCRIPTION JVM::WFF-CLASS-REP-STATIC))
 (36 36 (:REWRITE CAR-CONS))
 (36 12 (:DEFINITION JVM::FIELD-FIELDNAME-S))
 (24 24 (:TYPE-PRESCRIPTION JVM::WFF-STATE))
 (24 24 (:REWRITE SET-EQUAL-CONS))
 (24 24 (:REWRITE JVM::NO-FATAL-ERROR?-AFTER-LOAD-CLASS-X-IMPLIES-CLASS-EXISTS-EXTERNALLY))
 (24 24 (:DEFINITION JVM::MAKE-ARRAY-TYPE))
 (24 24 (:DEFINITION JVMFLOATP))
 (24 24 (:DEFINITION DOUBLEP))
 (12 12 (:TYPE-PRESCRIPTION JVM::LOADER-INV))
 (12 12 (:DEFINITION JVM::EXTERNAL-CLASS-TABLE))
 (8 8 (:TYPE-PRESCRIPTION JVM::ISASSIGNABLETO))
 )
