(BCV::COLLECT-SIG-FRAME-VECTOR-IS-SUFFIX
 (892 797 (:REWRITE DEFAULT-CAR))
 (589 495 (:REWRITE DEFAULT-CDR))
 (120 6 (:REWRITE JVM::TRUE-LISTP-LEN-1-IS-LIST-CAR))
 (42 42 (:TYPE-PRESCRIPTION LEN))
 (42 6 (:DEFINITION LEN))
 (24 24 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (12 6 (:REWRITE DEFAULT-+-2))
 (6 6 (:REWRITE DEL-SET-LEN))
 (6 6 (:REWRITE DEFAULT-+-1))
 )
(BCV::SIG-DO-INST-REDUCE-TO-COLLECT-SIG-FRAME-AT-MERGECODE1
 (120 102 (:REWRITE DEFAULT-CAR))
 (72 59 (:REWRITE DEFAULT-CDR))
 )
(BCV::SEARCHSTACKFRAME-REDUCE-SPECIFIC
 (64 2 (:DEFINITION BCV::STACK-MAP-WRAP))
 (46 2 (:DEFINITION BCV::MAKESTACKMAP))
 (44 2 (:REWRITE JVM::TRUE-LISTP-LEN-1-IS-LIST-CAR))
 (36 2 (:DEFINITION BCV::SEARCHSTACKFRAME))
 (25 15 (:REWRITE DEFAULT-CDR))
 (21 16 (:REWRITE DEFAULT-CAR))
 (19 2 (:DEFINITION LEN))
 (14 14 (:TYPE-PRESCRIPTION LEN))
 (12 4 (:DEFINITION BCV::GETMAP))
 (8 8 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (7 1 (:DEFINITION BCV::EXTRACT-FRAME-PC))
 (6 2 (:DEFINITION BCV::MAPFRAME))
 (4 4 (:TYPE-PRESCRIPTION BCV::STACK-MAP-WRAP))
 (4 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:TYPE-PRESCRIPTION BCV::ISSTACKMAPFRAME))
 (2 2 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (2 2 (:REWRITE DEL-SET-LEN))
 (2 2 (:REWRITE DEFAULT-+-1))
 (2 1 (:REWRITE DEFAULT-<-2))
 (2 1 (:REWRITE DEFAULT-<-1))
 )
(BCV::MEMBER-FRAME-ORDERED
 (490 388 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(BCV::COLLECT-SIG-FRAME-VECTOR-REDUCE
 (54 43 (:REWRITE DEFAULT-CDR))
 (20 1 (:REWRITE JVM::TRUE-LISTP-LEN-1-IS-LIST-CAR))
 (7 7 (:TYPE-PRESCRIPTION LEN))
 (7 1 (:DEFINITION LEN))
 (4 4 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (2 2 (:REWRITE BCV::MERGEDCODEISTYPESAFE-IMPLIES-NOT-END-CONSP))
 (2 1 (:REWRITE DEFAULT-+-2))
 (1 1 (:REWRITE DEL-SET-LEN))
 (1 1 (:REWRITE DEFAULT-+-1))
 )
(BCV::SEARCHSTACKFRAME-REDUCE-2
 (86 70 (:REWRITE DEFAULT-CDR))
 (40 2 (:REWRITE JVM::TRUE-LISTP-LEN-1-IS-LIST-CAR))
 (14 14 (:TYPE-PRESCRIPTION LEN))
 (14 2 (:DEFINITION LEN))
 (10 4 (:REWRITE BCV::COLLECT-SIG-FRAME-VECTOR-REDUCE))
 (8 8 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (4 4 (:REWRITE BCV::MERGEDCODEISTYPESAFE-IMPLIES-NOT-END-CONSP))
 (4 2 (:REWRITE DEFAULT-+-2))
 (3 3 (:REWRITE BCV::SEARCHSTACKFRAME-REDUCE))
 (2 2 (:REWRITE DEL-SET-LEN))
 (2 2 (:REWRITE DEFAULT-+-1))
 (1 1 (:REWRITE BCV::NEXT-PC-EXPAND-2))
 )
(BCV::COLLECT-SIG-FRAME-VECTOR-REDUCE-2
 (1086 1016 (:REWRITE DEFAULT-CDR))
 (277 17 (:REWRITE BCV::MERGEDCODEISTYPESAFE-MERGEDCODEISTYPE-SAFE))
 (243 29 (:REWRITE BCV::IS-SUFFIX-CDR))
 (236 56 (:REWRITE BCV::SIG-DO-INST-REDUCE-TO-COLLECT-SIG-FRAME-AT-MERGECODE1))
 (173 29 (:DEFINITION BCV::IS-SUFFIX))
 (94 94 (:TYPE-PRESCRIPTION BCV::IS-SUFFIX))
 (40 2 (:REWRITE JVM::TRUE-LISTP-LEN-1-IS-LIST-CAR))
 (18 14 (:REWRITE BCV::MERGEDCODEISTYPESAFE-IMPLIES-NOT-END-CONSP))
 (14 14 (:TYPE-PRESCRIPTION LEN))
 (14 2 (:DEFINITION LEN))
 (8 8 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (4 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:REWRITE DEL-SET-LEN))
 (2 2 (:REWRITE DEFAULT-+-1))
 )
(BCV::SEARCHSTACKFRAME-REDUCE-3
 (1663 1555 (:REWRITE DEFAULT-CDR))
 (240 12 (:REWRITE JVM::TRUE-LISTP-LEN-1-IS-LIST-CAR))
 (133 40 (:REWRITE BCV::COLLECT-SIG-FRAME-VECTOR-REDUCE))
 (130 43 (:REWRITE BCV::COLLECT-SIG-FRAME-VECTOR-REDUCE-2))
 (84 84 (:TYPE-PRESCRIPTION LEN))
 (84 12 (:DEFINITION LEN))
 (48 48 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (47 43 (:REWRITE BCV::MERGEDCODEISTYPESAFE-IMPLIES-NOT-END-CONSP))
 (38 38 (:REWRITE BCV::SEARCHSTACKFRAME-REDUCE))
 (24 12 (:REWRITE DEFAULT-+-2))
 (12 12 (:REWRITE DEL-SET-LEN))
 (12 12 (:REWRITE DEFAULT-+-1))
 )
(BCV::SEARCHSTACKFRAME-REDUCE-4
 (1744 1640 (:REWRITE DEFAULT-CDR))
 (240 12 (:REWRITE JVM::TRUE-LISTP-LEN-1-IS-LIST-CAR))
 (128 41 (:REWRITE BCV::COLLECT-SIG-FRAME-VECTOR-REDUCE))
 (119 41 (:REWRITE BCV::COLLECT-SIG-FRAME-VECTOR-REDUCE-2))
 (101 35 (:REWRITE BCV::SEARCHSTACKFRAME-REDUCE-3))
 (84 84 (:TYPE-PRESCRIPTION LEN))
 (84 12 (:DEFINITION LEN))
 (48 48 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (45 41 (:REWRITE BCV::MERGEDCODEISTYPESAFE-IMPLIES-NOT-END-CONSP))
 (36 36 (:REWRITE BCV::SEARCHSTACKFRAME-REDUCE))
 (24 12 (:REWRITE DEFAULT-+-2))
 (12 12 (:REWRITE DEL-SET-LEN))
 (12 12 (:REWRITE DEFAULT-+-1))
 )
(BCV::WFF-MERGEDCODE-OFFSET-TR-IMPLIES-CONSP-EXTRACT-PC
 (361 361 (:REWRITE DEFAULT-CAR))
 (154 154 (:REWRITE DEFAULT-CDR))
 )
(BCV::ORDERED-IMPLIES-LESS-NOT-EQUAL
 (744 623 (:REWRITE DEFAULT-CAR))
 (452 32 (:REWRITE BCV::WFF-MERGEDCODE-OFFSET-TR-IMPLIES-CONSP-EXTRACT-PC))
 (392 14 (:DEFINITION BCV::WFF-STACK-MAP-OFFSET))
 (301 293 (:REWRITE DEFAULT-CDR))
 (298 229 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (264 138 (:REWRITE DEFAULT-<-1))
 (98 98 (:TYPE-PRESCRIPTION BCV::WFF-STACK-MAP-OFFSET))
 (98 14 (:DEFINITION BCV::NEXT-PC))
 )
(BCV::COLLECT-SIG-FRAME-VECTOR-COLLECT-LAST-FRAME-LEMMA
 (6940 6602 (:REWRITE DEFAULT-CAR))
 (5251 4970 (:REWRITE DEFAULT-CDR))
 (1257 93 (:REWRITE BCV::COLLECT-SIG-FRAME-VECTOR-REDUCE-2))
 (837 79 (:REWRITE BCV::SEARCHSTACKFRAME-REDUCE-3))
 (697 93 (:REWRITE BCV::MERGEDCODEISTYPESAFE-IMPLIES-NOT-END-CONSP))
 (573 447 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (474 72 (:REWRITE BCV::COLLECT-SIG-FRAME-VECTOR-REDUCE))
 (100 3 (:REWRITE BCV::SEARCHSTACKFRAME-REDUCE-SPECIFIC))
 (79 3 (:DEFINITION BCV::PC-WFF-MERGEDCODE1))
 (64 64 (:REWRITE BCV::SEARCHSTACKFRAME-REDUCE))
 (18 18 (:TYPE-PRESCRIPTION JVM::INST-SIZE))
 (15 15 (:TYPE-PRESCRIPTION BCV::PC-WFF-MERGEDCODE1))
 (11 3 (:REWRITE COMMUTATIVITY-OF-+))
 (8 6 (:REWRITE DEFAULT-+-2))
 (8 6 (:REWRITE DEFAULT-+-1))
 )
(BCV::COLLECT-SIG-FRAME-VECTOR-COLLECT-LAST-FRAME
 (38 37 (:REWRITE DEFAULT-CAR))
 (36 1 (:DEFINITION BCV::PC-WFF-MERGEDCODE1))
 (32 1 (:DEFINITION BCV::COLLECT-SIG-FRAME-VECTOR))
 (31 1 (:DEFINITION BCV::MERGEDCODEISTYPESAFE))
 (30 28 (:REWRITE DEFAULT-CDR))
 (20 4 (:DEFINITION MV-NTH))
 (18 2 (:DEFINITION BCV::NEXT-STACKFRAME))
 (14 1 (:DEFINITION BCV::STACK-MAP-WRAP))
 (7 7 (:TYPE-PRESCRIPTION BCV::SIG-DO-INST))
 (6 2 (:REWRITE BCV::SIG-DO-INST-REDUCE-TO-COLLECT-SIG-FRAME-AT-MERGECODE1))
 (6 2 (:DEFINITION BCV::SUFFIX))
 (5 1 (:REWRITE BCV::MERGEDCODEISTYPESAFE-MERGEDCODEISTYPE-SAFE))
 (4 4 (:TYPE-PRESCRIPTION BCV::SUFFIX))
 (4 1 (:REWRITE COMMUTATIVITY-OF-+))
 (4 1 (:REWRITE BCV::COLLECT-SIG-FRAME-VECTOR-REDUCE-2))
 (4 1 (:REWRITE BCV::COLLECT-SIG-FRAME-VECTOR-REDUCE))
 (3 3 (:TYPE-PRESCRIPTION JVM::INST-SIZE))
 (3 2 (:REWRITE DEFAULT-<-1))
 (3 2 (:REWRITE DEFAULT-+-2))
 (3 2 (:REWRITE DEFAULT-+-1))
 (3 1 (:REWRITE BCV::IS-SUFFIX-CDR))
 (3 1 (:DEFINITION MEMBER-EQUAL))
 (2 2 (:TYPE-PRESCRIPTION BCV::IS-SUFFIX))
 (2 2 (:TYPE-PRESCRIPTION BCV::INSTRUCTIONSATISFIESHANDLERS))
 (2 2 (:TYPE-PRESCRIPTION BCV::FRAMEISASSIGNABLE))
 (2 2 (:TYPE-PRESCRIPTION BCV::COLLECT-SIG-FRAME-VECTOR))
 (2 2 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (2 2 (:REWRITE DEFAULT-<-2))
 (2 1 (:DEFINITION NTH))
 (1 1 (:REWRITE BCV::MERGEDCODEISTYPESAFE-IMPLIES-NOT-END-CONSP))
 (1 1 (:DEFINITION BCV::IS-SUFFIX))
 )
(BCV::IS-SUFFIX-NOT-NIL-MEMBER
 (12 2 (:DEFINITION BCV::IS-SUFFIX))
 (8 8 (:REWRITE DEFAULT-CDR))
 (8 8 (:REWRITE DEFAULT-CAR))
 (6 2 (:DEFINITION MEMBER-EQUAL))
 )
(BCV::ISSTACKMAP-CAR-CONSP)
(BCV::PC-WFF-MERGEDCODE1-ISSTACKMAP-CONSP-REMAINING-SPECIFIC
 (4 4 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE DEFAULT-CDR))
 )
(BCV::PC-WFF-MERGEDCODE1-MAPOFFSET-EQUAL-FORWARD-TO-NEXT-INST
 (89 64 (:REWRITE DEFAULT-+-1))
 (64 64 (:REWRITE DEFAULT-<-2))
 (64 64 (:REWRITE DEFAULT-<-1))
 (44 44 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(BCV::SEARCHSTACKFRAME-IS-IF-STACK-MAP
 (84 83 (:REWRITE DEFAULT-CAR))
 (67 63 (:REWRITE DEFAULT-CDR))
 (64 2 (:DEFINITION BCV::STACK-MAP-WRAP))
 (64 2 (:DEFINITION BCV::COLLECT-SIG-FRAME-VECTOR))
 (36 2 (:DEFINITION BCV::MAKESTACKMAP))
 (34 2 (:REWRITE JVM::TRUE-LISTP-LEN-1-IS-LIST-CAR))
 (28 4 (:DEFINITION BCV::NEXT-STACKFRAME))
 (14 14 (:TYPE-PRESCRIPTION LEN))
 (10 6 (:REWRITE DEFAULT-+-2))
 (10 2 (:DEFINITION LEN))
 (9 3 (:DEFINITION BCV::SUFFIX))
 (8 8 (:LINEAR SUBSET-NODUP-SET-SIZE))
 (8 6 (:REWRITE DEFAULT-+-1))
 (8 2 (:REWRITE BCV::COLLECT-SIG-FRAME-VECTOR-REDUCE-2))
 (8 2 (:REWRITE BCV::COLLECT-SIG-FRAME-VECTOR-REDUCE))
 (6 4 (:REWRITE DEFAULT-<-1))
 (4 4 (:TYPE-PRESCRIPTION BCV::COLLECT-SIG-FRAME-VECTOR))
 (4 4 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (4 4 (:REWRITE DEFAULT-<-2))
 (4 2 (:DEFINITION NTH))
 (2 2 (:REWRITE BCV::MERGEDCODEISTYPESAFE-IMPLIES-NOT-END-CONSP))
 (2 2 (:REWRITE DEL-SET-LEN))
 )
