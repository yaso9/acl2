(ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR
 (451 451 (:REWRITE DEFAULT-CAR))
 (159 159 (:REWRITE DEFAULT-CDR))
 )
(CHANNEL-HEADERP)
(CHANNEL-HEADERP-OF-LIST
 (10 10 (:REWRITE DEFAULT-CDR))
 (6 3 (:REWRITE DEFAULT-+-2))
 (4 4 (:REWRITE DEFAULT-CAR))
 (3 3 (:TYPE-PRESCRIPTION LEN))
 (3 3 (:REWRITE DEFAULT-+-1))
 )
(STRINGP-OF-CADDR-WHEN-CHANNEL-HEADERP)
(INTEGERP-OF-CADDDR-WHEN-CHANNEL-HEADERP)
(TYPED-IO-LISTP-OF-CDR-AND-CADR-OF-CAR)
(TYPED-IO-LISTP-OF-CDR-GEN
 (2 2 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE DEFAULT-CAR))
 )
(OPEN-CHANNEL-LISTP-OF-ADD-PAIR-STRONG
 (55 51 (:REWRITE DEFAULT-CAR))
 (43 30 (:REWRITE DEFAULT-CDR))
 (35 7 (:REWRITE SYMBOL<-ASYMMETRIC))
 (18 15 (:REWRITE SYMBOL<-TRANSITIVE))
 (15 15 (:REWRITE SYMBOL<-TRICHOTOMY))
 )
(OPEN-CHANNEL1-OF-CDR-OF-ASSOC-EQUAL
 (1003 16 (:REWRITE ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR))
 (962 28 (:DEFINITION ORDERED-SYMBOL-ALISTP))
 (324 81 (:REWRITE SYMBOL<-TRICHOTOMY))
 (310 310 (:REWRITE DEFAULT-CAR))
 (216 216 (:TYPE-PRESCRIPTION SYMBOL<))
 (187 187 (:REWRITE DEFAULT-CDR))
 (162 27 (:REWRITE SYMBOL<-ASYMMETRIC))
 (130 130 (:TYPE-PRESCRIPTION ORDERED-SYMBOL-ALISTP))
 (81 81 (:REWRITE SYMBOL<-TRANSITIVE))
 )
(ORDERED-SYMBOL-ALISTP-OF-ADD-PAIR
 (776 719 (:REWRITE DEFAULT-CAR))
 (569 478 (:REWRITE SYMBOL<-TRANSITIVE))
 (327 299 (:REWRITE DEFAULT-CDR))
 )
(OPEN-CHANNELS-P-OF-ADD-PAIR-STRONG
 (42 1 (:DEFINITION ORDERED-SYMBOL-ALISTP))
 (19 5 (:REWRITE SYMBOL<-TRICHOTOMY))
 (17 3 (:REWRITE SYMBOL<-ASYMMETRIC))
 (12 12 (:TYPE-PRESCRIPTION SYMBOL<))
 (10 10 (:REWRITE DEFAULT-CAR))
 (6 6 (:REWRITE DEFAULT-CDR))
 (5 5 (:REWRITE SYMBOL<-TRANSITIVE))
 )
(OPEN-CHANNEL1-OF-CONS
 (43 43 (:REWRITE DEFAULT-CDR))
 (23 23 (:REWRITE DEFAULT-CAR))
 (16 8 (:REWRITE DEFAULT-+-2))
 (8 8 (:REWRITE DEFAULT-+-1))
 (2 2 (:REWRITE DEFAULT-COERCE-2))
 (2 2 (:REWRITE DEFAULT-COERCE-1))
 )
(CHANNEL-HEADERP-OF-CADR-OF-ASSOC-EQUAL-IFF
 (2060 51 (:REWRITE ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR))
 (1932 54 (:DEFINITION ORDERED-SYMBOL-ALISTP))
 (894 894 (:REWRITE DEFAULT-CAR))
 (712 712 (:REWRITE DEFAULT-CDR))
 (636 159 (:REWRITE SYMBOL<-TRICHOTOMY))
 (424 424 (:TYPE-PRESCRIPTION SYMBOL<))
 (318 53 (:REWRITE SYMBOL<-ASYMMETRIC))
 (312 312 (:TYPE-PRESCRIPTION ORDERED-SYMBOL-ALISTP))
 (159 159 (:REWRITE SYMBOL<-TRANSITIVE))
 (110 55 (:REWRITE DEFAULT-+-2))
 (55 55 (:REWRITE DEFAULT-+-1))
 (3 1 (:DEFINITION CHARACTER-LISTP))
 )
(CHANNEL-HEADERP-OF-CADR-OF-ASSOC-EQUAL-IFF-2
 (182 5 (:DEFINITION ORDERED-SYMBOL-ALISTP))
 (174 8 (:REWRITE ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR))
 (70 20 (:REWRITE SYMBOL<-TRICHOTOMY))
 (69 69 (:REWRITE DEFAULT-CAR))
 (41 7 (:REWRITE SYMBOL<-ASYMMETRIC))
 (35 35 (:REWRITE DEFAULT-CDR))
 (20 20 (:REWRITE SYMBOL<-TRANSITIVE))
 )
(TYPED-IO-LISTP-OF-CDDDR-OF-ASSOC-EQUAL-AND-CADR-OF-CADR-OF-ASSOC-EQUAL
 (396 14 (:REWRITE ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR))
 (366 11 (:DEFINITION ORDERED-SYMBOL-ALISTP))
 (218 218 (:REWRITE DEFAULT-CAR))
 (176 176 (:REWRITE DEFAULT-CDR))
 (120 30 (:REWRITE SYMBOL<-TRICHOTOMY))
 (84 28 (:REWRITE TYPED-IO-LISTP-OF-CDR-GEN))
 (80 80 (:TYPE-PRESCRIPTION SYMBOL<))
 (61 61 (:TYPE-PRESCRIPTION ORDERED-SYMBOL-ALISTP))
 (60 10 (:REWRITE SYMBOL<-ASYMMETRIC))
 (36 18 (:REWRITE DEFAULT-+-2))
 (30 30 (:REWRITE SYMBOL<-TRANSITIVE))
 (18 18 (:REWRITE DEFAULT-+-1))
 (16 16 (:REWRITE DEFAULT-<-2))
 (16 16 (:REWRITE DEFAULT-<-1))
 )
(TYPED-IO-LISTP-OF-CDDDR-OF-ASSOC-EQUAL-AND-CADR-OF-CADR-OF-ASSOC-EQUAL-2
 (112 3 (:DEFINITION ORDERED-SYMBOL-ALISTP))
 (86 2 (:DEFINITION ASSOC-EQUAL))
 (84 4 (:REWRITE ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR))
 (44 12 (:REWRITE SYMBOL<-TRICHOTOMY))
 (40 40 (:REWRITE DEFAULT-CAR))
 (29 5 (:REWRITE SYMBOL<-ASYMMETRIC))
 (25 25 (:REWRITE DEFAULT-CDR))
 (12 12 (:REWRITE SYMBOL<-TRANSITIVE))
 )
(EQUAL-OF-ADD-PAIR-SAME
 (694 35 (:REWRITE ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR))
 (597 589 (:REWRITE DEFAULT-CAR))
 (580 195 (:REWRITE SYMBOL<-TRICHOTOMY))
 (479 89 (:REWRITE SYMBOL<-ASYMMETRIC))
 (305 291 (:REWRITE DEFAULT-CDR))
 (192 190 (:REWRITE SYMBOL<-TRANSITIVE))
 (80 2 (:REWRITE ORDERED-SYMBOL-ALISTP-OF-ADD-PAIR))
 (80 2 (:REWRITE ORDERED-SYMBOL-ALISTP-ADD-PAIR))
 (24 2 (:REWRITE OPEN-CHANNEL-LISTP-OF-ADD-PAIR-STRONG))
 )
(TRUE-LIST-OF-CDDR-OF-ASSOC-EQUAL-WHEN-OPEN-CHANNEL-LISTP
 (607 11 (:REWRITE ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR))
 (578 17 (:DEFINITION ORDERED-SYMBOL-ALISTP))
 (389 389 (:REWRITE DEFAULT-CAR))
 (307 307 (:REWRITE DEFAULT-CDR))
 (192 48 (:REWRITE SYMBOL<-TRICHOTOMY))
 (128 128 (:TYPE-PRESCRIPTION SYMBOL<))
 (96 16 (:REWRITE SYMBOL<-ASYMMETRIC))
 (89 8 (:REWRITE TYPED-IO-LISTP-OF-CDR-GEN))
 (89 8 (:REWRITE TYPED-IO-LISTP-OF-CDR-AND-CADR-OF-CAR))
 (83 83 (:TYPE-PRESCRIPTION ORDERED-SYMBOL-ALISTP))
 (48 48 (:REWRITE SYMBOL<-TRANSITIVE))
 (40 8 (:DEFINITION LEN))
 (35 35 (:TYPE-PRESCRIPTION OPEN-CHANNEL1))
 (30 15 (:DEFINITION TRUE-LISTP))
 (16 8 (:REWRITE DEFAULT-+-2))
 (12 12 (:TYPE-PRESCRIPTION MEMBER-EQUAL))
 (8 8 (:REWRITE DEFAULT-+-1))
 )
(NAT-LISTP-OF-CDDR-OF-ASSOC-EQUAL-WHEN-OPEN-CHANNEL-LISTP
 (2255 42 (:REWRITE ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR))
 (2139 60 (:DEFINITION ORDERED-SYMBOL-ALISTP))
 (1362 27 (:REWRITE TYPED-IO-LISTP-OF-CDR-GEN))
 (997 997 (:REWRITE DEFAULT-CAR))
 (730 730 (:REWRITE DEFAULT-CDR))
 (708 177 (:REWRITE SYMBOL<-TRICHOTOMY))
 (629 30 (:DEFINITION TRUE-LISTP))
 (472 472 (:TYPE-PRESCRIPTION SYMBOL<))
 (451 19 (:DEFINITION LEN))
 (354 59 (:REWRITE SYMBOL<-ASYMMETRIC))
 (324 324 (:TYPE-PRESCRIPTION ORDERED-SYMBOL-ALISTP))
 (177 177 (:REWRITE SYMBOL<-TRANSITIVE))
 (95 10 (:REWRITE TYPED-IO-LISTP-OF-CDR-AND-CADR-OF-CAR))
 (60 60 (:TYPE-PRESCRIPTION OPEN-CHANNEL1))
 (42 21 (:REWRITE DEFAULT-+-2))
 (38 38 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (21 21 (:REWRITE DEFAULT-+-1))
 (18 18 (:TYPE-PRESCRIPTION MEMBER-EQUAL))
 (14 6 (:REWRITE OPEN-CHANNEL1-OF-CDR-OF-ASSOC-EQUAL))
 (13 7 (:REWRITE TRUE-LIST-OF-CDDR-OF-ASSOC-EQUAL-WHEN-OPEN-CHANNEL-LISTP))
 (6 6 (:REWRITE DEFAULT-<-2))
 (6 6 (:REWRITE DEFAULT-<-1))
 (2 2 (:REWRITE DEFAULT-COERCE-2))
 (2 2 (:REWRITE DEFAULT-COERCE-1))
 )
(UNSIGNED-BYTE-LISTP-OF-CDDR-OF-ASSOC-EQUAL-WHEN-OPEN-CHANNEL-LISTP
 (2255 42 (:REWRITE ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR))
 (2139 60 (:DEFINITION ORDERED-SYMBOL-ALISTP))
 (1330 21 (:REWRITE TYPED-IO-LISTP-OF-CDR-GEN))
 (985 985 (:REWRITE DEFAULT-CAR))
 (723 723 (:REWRITE DEFAULT-CDR))
 (708 177 (:REWRITE SYMBOL<-TRICHOTOMY))
 (627 29 (:DEFINITION TRUE-LISTP))
 (472 472 (:TYPE-PRESCRIPTION SYMBOL<))
 (451 19 (:DEFINITION LEN))
 (354 59 (:REWRITE SYMBOL<-ASYMMETRIC))
 (324 324 (:TYPE-PRESCRIPTION ORDERED-SYMBOL-ALISTP))
 (177 177 (:REWRITE SYMBOL<-TRANSITIVE))
 (95 10 (:REWRITE TYPED-IO-LISTP-OF-CDR-AND-CADR-OF-CAR))
 (54 54 (:TYPE-PRESCRIPTION OPEN-CHANNEL1))
 (42 21 (:REWRITE DEFAULT-+-2))
 (34 34 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (21 21 (:REWRITE DEFAULT-+-1))
 (18 18 (:TYPE-PRESCRIPTION MEMBER-EQUAL))
 (14 6 (:REWRITE OPEN-CHANNEL1-OF-CDR-OF-ASSOC-EQUAL))
 (11 7 (:REWRITE TRUE-LIST-OF-CDDR-OF-ASSOC-EQUAL-WHEN-OPEN-CHANNEL-LISTP))
 (2 2 (:REWRITE DEFAULT-COERCE-2))
 (2 2 (:REWRITE DEFAULT-COERCE-1))
 )
(CHARACTER-LISTP-OF-CDDR-OF-ASSOC-EQUAL-WHEN-OPEN-CHANNEL-LISTP
 (1455 22 (:REWRITE ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR))
 (1399 40 (:DEFINITION ORDERED-SYMBOL-ALISTP))
 (700 700 (:REWRITE DEFAULT-CAR))
 (495 495 (:REWRITE DEFAULT-CDR))
 (468 117 (:REWRITE SYMBOL<-TRICHOTOMY))
 (312 312 (:TYPE-PRESCRIPTION SYMBOL<))
 (234 39 (:REWRITE SYMBOL<-ASYMMETRIC))
 (184 184 (:TYPE-PRESCRIPTION ORDERED-SYMBOL-ALISTP))
 (117 117 (:REWRITE SYMBOL<-TRANSITIVE))
 (93 9 (:REWRITE TYPED-IO-LISTP-OF-CDR-GEN))
 (93 9 (:REWRITE TYPED-IO-LISTP-OF-CDR-AND-CADR-OF-CAR))
 (45 9 (:DEFINITION LEN))
 (41 41 (:TYPE-PRESCRIPTION OPEN-CHANNEL1))
 (28 14 (:DEFINITION TRUE-LISTP))
 (18 9 (:REWRITE DEFAULT-+-2))
 (12 12 (:TYPE-PRESCRIPTION MEMBER-EQUAL))
 (9 9 (:REWRITE DEFAULT-+-1))
 )
(OPEN-CHANNEL-LISTP-OF-CONS
 (8 8 (:REWRITE DEFAULT-CDR))
 (3 3 (:REWRITE DEFAULT-CAR))
 )
(ORDERED-SYMBOL-ALISTP-OF-REMOVE1-ASSOC-EQUAL)
(OPEN-CHANNEL-LISTP-OF-REMOVE1-ASSOC-EQUAL-ALT
 (28 28 (:REWRITE DEFAULT-CAR))
 (24 24 (:REWRITE DEFAULT-CDR))
 )
(OPEN-CHANNELS-P-OF-REMOVE1-ASSOC-EQUAL-ALT)
(OPEN-INPUT-CHANNEL-P1-FORWARD-TO-ASSOC-EQUAL)
(SYMBOLP-WHEN-ASSOC-EQUAL-AND-OPEN-CHANNELS-P
 (256 19 (:REWRITE ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR))
 (107 107 (:REWRITE DEFAULT-CDR))
 (85 85 (:REWRITE SYMBOL<-TRANSITIVE))
 )
(SYMBOLP-WHEN-ASSOC-EQUAL-OF-OPEN-INPUT-CHANNELS-AND-STATE-P1
 (148 16 (:DEFINITION MEMBER-EQUAL))
 (141 141 (:REWRITE DEFAULT-CAR))
 (135 135 (:REWRITE DEFAULT-CDR))
 (80 40 (:DEFINITION NTH))
 (78 1 (:DEFINITION WRITTEN-FILE-LISTP))
 (74 1 (:DEFINITION READABLE-FILES-LISTP))
 (73 1 (:DEFINITION WRITTEN-FILE))
 (69 1 (:DEFINITION READABLE-FILE))
 (59 1 (:DEFINITION READ-FILE-LISTP))
 (55 10 (:REWRITE ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR))
 (55 1 (:DEFINITION WRITABLE-FILE-LISTP))
 (55 1 (:DEFINITION READ-FILE-LISTP1))
 (51 1 (:DEFINITION WRITABLE-FILE-LISTP1))
 (40 5 (:DEFINITION ASSOC-EQUAL))
 (25 5 (:DEFINITION LEN))
 (22 2 (:DEFINITION FGETPROP))
 (18 9 (:DEFINITION TRUE-LISTP))
 (15 1 (:DEFINITION KNOWN-PACKAGE-ALISTP))
 (12 2 (:DEFINITION SYMBOL-ALISTP))
 (12 1 (:DEFINITION TIMER-ALISTP))
 (12 1 (:DEFINITION PLIST-WORLDP))
 (10 5 (:REWRITE SYMBOL<-TRICHOTOMY))
 (10 5 (:REWRITE DEFAULT-+-2))
 (8 8 (:TYPE-PRESCRIPTION OPEN-CHANNEL1))
 (6 2 (:REWRITE TYPED-IO-LISTP-OF-CDR-GEN))
 (6 2 (:REWRITE TYPED-IO-LISTP-OF-CDR-AND-CADR-OF-CAR))
 (5 5 (:REWRITE SYMBOL<-TRANSITIVE))
 (5 5 (:REWRITE DEFAULT-+-1))
 (4 1 (:DEFINITION SYMBOL-LISTP))
 (3 1 (:DEFINITION RATIONAL-LISTP))
 (3 1 (:DEFINITION INTEGER-LISTP))
 (2 2 (:TYPE-PRESCRIPTION TYPED-IO-LISTP))
 (1 1 (:TYPE-PRESCRIPTION SYMBOL-LISTP))
 (1 1 (:REWRITE DEFAULT-<-2))
 (1 1 (:REWRITE DEFAULT-<-1))
 )
(SYMBOLP-WHEN-ASSOC-EQUAL-OF-OPEN-INPUT-CHANNELS-AND-STATE-P
 (8 1 (:DEFINITION ASSOC-EQUAL))
 (6 2 (:REWRITE STATE-P-IMPLIES-AND-FORWARD-TO-STATE-P1))
 (6 2 (:REWRITE ASSOC-EQUAL-WHEN-ORDERED-SYMBOL-ALISTP-AND-SYMBOL<-OF-CAAR))
 (3 3 (:REWRITE DEFAULT-CAR))
 (1 1 (:REWRITE DEFAULT-CDR))
 )
(ASSOC-EQUAL-OF-OPEN-INPUT-CHANNELS-WHEN-OPEN-INPUT-CHANNEL-P)