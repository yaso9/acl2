(SV::LOGBITP-OF-LOGEQV
 (64 24 (:REWRITE BITOPS::LOGBITP-WHEN-BITMASKP))
 (49 19 (:REWRITE BITOPS::LOGBITP-NONZERO-OF-BIT))
 (48 48 (:TYPE-PRESCRIPTION BITMASKP$INLINE))
 (30 30 (:TYPE-PRESCRIPTION BITP))
 (24 24 (:REWRITE BITOPS::LOGBITP-OF-NEGATIVE-CONST))
 (24 24 (:REWRITE BITOPS::LOGBITP-OF-MASK))
 (24 24 (:REWRITE BITOPS::LOGBITP-OF-CONST))
 (24 24 (:META BITOPS::OPEN-LOGBITP-OF-CONST-LITE-META))
 (18 2 (:REWRITE BFIX-WHEN-NOT-1))
 (8 8 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (8 8 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (8 8 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (6 2 (:REWRITE BITOPS::LOGAND-WITH-NEGATED-BITMASK))
 (6 2 (:REWRITE BITOPS::LOGAND-WITH-BITMASK))
 (4 4 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (4 4 (:REWRITE BITOPS::B-IOR-EQUAL-1-IN-CONCL))
 (4 2 (:REWRITE BFIX-WHEN-NOT-BITP))
 (4 2 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (4 2 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (4 2 (:LINEAR BITOPS::B-IOR-BOUND))
 (1 1 (:REWRITE BITOPS::B-AND-EQUAL-1-IN-HYP))
 )
(SV::LOGHEAD-LEMMA
 (3274 178 (:REWRITE LOGHEAD-IDENTITY))
 (2752 172 (:DEFINITION UNSIGNED-BYTE-P))
 (2236 172 (:DEFINITION INTEGER-RANGE-P))
 (2105 423 (:REWRITE BITOPS::LOGBITP-WHEN-BIT))
 (1456 1456 (:TYPE-PRESCRIPTION BITP))
 (1324 840 (:REWRITE DEFAULT-<-2))
 (1259 423 (:REWRITE BITOPS::LOGBITP-WHEN-BITMASKP))
 (1040 840 (:REWRITE DEFAULT-<-1))
 (935 315 (:REWRITE BITOPS::LOGBITP-NONZERO-OF-BIT))
 (924 924 (:TYPE-PRESCRIPTION BITMASKP$INLINE))
 (700 5 (:LINEAR BITOPS::LOGIOR->=-0-LINEAR))
 (700 5 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-1))
 (688 688 (:TYPE-PRESCRIPTION RATIONALP-EXPT-TYPE-PRESCRIPTION))
 (688 688 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NON-ZERO-BASE))
 (542 271 (:TYPE-PRESCRIPTION BITOPS::LOGNOT-NEGP))
 (542 271 (:TYPE-PRESCRIPTION BITOPS::LOGNOT-NATP))
 (538 186 (:REWRITE NFIX-WHEN-NOT-NATP))
 (440 5 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-2))
 (423 423 (:REWRITE BITOPS::LOGBITP-OF-NEGATIVE-CONST))
 (423 423 (:REWRITE BITOPS::LOGBITP-OF-MASK))
 (423 423 (:REWRITE BITOPS::LOGBITP-OF-CONST-SPLIT))
 (423 423 (:REWRITE BITOPS::LOGBITP-OF-CONST))
 (418 198 (:REWRITE NATP-WHEN-GTE-0))
 (390 15 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-2))
 (320 320 (:TYPE-PRESCRIPTION BITOPS::LOGAND-NATP-TYPE-1))
 (291 291 (:TYPE-PRESCRIPTION NEGP))
 (290 15 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 2))
 (250 80 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (250 80 (:REWRITE IFIX-WHEN-INTEGERP))
 (198 198 (:REWRITE NATP-WHEN-INTEGERP))
 (180 20 (:REWRITE BITOPS::LOGNOT-<-CONST))
 (180 15 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-1))
 (180 15 (:LINEAR BITOPS::LOGAND-<-0-LINEAR))
 (172 172 (:TYPE-PRESCRIPTION UNSIGNED-BYTE-P))
 (172 172 (:REWRITE BITOPS::UNSIGNED-BYTE-P-INCR))
 (124 124 (:REWRITE INEQUALITY-WITH-NFIX-HYP-1))
 (120 20 (:REWRITE IFIX-NEGATIVE-TO-NEGP))
 (96 96 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (96 48 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (96 48 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (80 40 (:REWRITE BFIX-WHEN-NOT-BITP))
 (80 15 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 1))
 (66 22 (:REWRITE BITOPS::LOGAND-WITH-NEGATED-BITMASK))
 (66 22 (:REWRITE BITOPS::LOGAND-WITH-BITMASK))
 (60 60 (:REWRITE INEQUALITY-WITH-NFIX-HYP-2))
 (60 20 (:REWRITE NEGP-WHEN-LESS-THAN-0))
 (42 42 (:REWRITE BITOPS::B-AND-EQUAL-1-IN-HYP))
 (35 35 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (31 31 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (31 31 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (20 20 (:REWRITE NEGP-WHEN-INTEGERP))
 (12 12 (:REWRITE BITOPS::B-IOR-EQUAL-1-IN-CONCL))
 (10 10 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(SV::SVEX-ENV-LOOKUP-WHEN-2VEC-P-AND-<<=)
(SV::ZERO-EXT-EQUAL-WHEN-4VEC-<<=-AND-2VEC-P
 (382 2 (:LINEAR BITOPS::LOGIOR->=-0-LINEAR))
 (382 2 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-1))
 (330 18 (:REWRITE LOGHEAD-IDENTITY))
 (286 13 (:DEFINITION UNSIGNED-BYTE-P))
 (273 13 (:DEFINITION INTEGER-RANGE-P))
 (245 102 (:TYPE-PRESCRIPTION BITOPS::LOGNOT-NATP))
 (212 6 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-2))
 (176 2 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-2))
 (165 69 (:REWRITE DEFAULT-<-1))
 (160 40 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (160 6 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 2))
 (136 136 (:TYPE-PRESCRIPTION BITOPS::LOGAND-NATP-TYPE-1))
 (103 69 (:REWRITE DEFAULT-<-2))
 (91 91 (:TYPE-PRESCRIPTION NEGP))
 (88 8 (:REWRITE BITOPS::LOGNOT-<-CONST))
 (72 6 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-1))
 (72 6 (:LINEAR BITOPS::LOGAND-<-0-LINEAR))
 (52 52 (:TYPE-PRESCRIPTION RATIONALP-EXPT-TYPE-PRESCRIPTION))
 (52 52 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NON-ZERO-BASE))
 (48 6 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 1))
 (28 28 (:TYPE-PRESCRIPTION BITMASKP$INLINE))
 (21 7 (:REWRITE BITOPS::LOGAND-WITH-NEGATED-BITMASK))
 (21 7 (:REWRITE BITOPS::LOGAND-WITH-BITMASK))
 (13 13 (:TYPE-PRESCRIPTION UNSIGNED-BYTE-P))
 (13 13 (:REWRITE BITOPS::UNSIGNED-BYTE-P-INCR))
 (4 4 (:REWRITE SV::4VEC-P-WHEN-MEMBER-EQUAL-OF-4VECLIST-P))
 (2 2 (:REWRITE-QUOTED-CONSTANT SV::MAYBE-4VEC-FIX-UNDER-MAYBE-4VEC-EQUIV))
 (2 2 (:REWRITE-QUOTED-CONSTANT SV::4VEC-FIX-UNDER-4VEC-EQUIV))
 )
(SV::ZERO-EXT-EQUAL-WHEN-4VEC-<<=-AND-INTEGERP
 (38 1 (:REWRITE SV::4VEC-<<=-2VEC))
 (17 3 (:REWRITE INTEGERP-OF-CAR-WHEN-INTEGER-LISTP))
 (17 2 (:REWRITE IFIX-WHEN-INTEGERP))
 (11 1 (:DEFINITION INTEGER-LISTP))
 (8 8 (:TYPE-PRESCRIPTION INTEGER-LISTP))
 (8 8 (:TYPE-PRESCRIPTION IFIX))
 (6 2 (:REWRITE SV::4VEC->LOWER-WHEN-2VEC-P))
 (4 2 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (2 2 (:REWRITE INTEGER-LISTP-WHEN-NOT-CONSP))
 (2 2 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 1 (:REWRITE INTEGER-LISTP-OF-CDR-WHEN-INTEGER-LISTP))
 )
(SV::ZERO-EXT-OF-SVEX-ENV-LOOKUP-WHEN-INTEGERP-AND-<<=
 (13 1 (:REWRITE SV::4VEC-<<=-2VEC))
 (11 1 (:DEFINITION SV::2VEC-P$INLINE))
 (5 5 (:TYPE-PRESCRIPTION SV::INTEGERP-OF-4VEC->UPPER))
 (3 3 (:TYPE-PRESCRIPTION SV::INTEGERP-OF-4VEC->LOWER))
 (2 2 (:TYPE-PRESCRIPTION SV::2VEC-P$INLINE))
 (2 1 (:REWRITE SV::4VEC->LOWER-WHEN-2VEC-P))
 )
(SV::LOGHEAD-LOGTAIL-LEMMA
 (14796 548 (:DEFINITION UNSIGNED-BYTE-P))
 (13152 548 (:DEFINITION INTEGER-RANGE-P))
 (12558 286 (:REWRITE LOGHEAD-IDENTITY))
 (5218 286 (:REWRITE LOGTAIL-IDENTITY))
 (5208 2604 (:TYPE-PRESCRIPTION BITOPS::LOGTAIL-NATP))
 (4068 682 (:REWRITE BITOPS::LOGBITP-WHEN-BIT))
 (3859 2581 (:REWRITE DEFAULT-<-2))
 (3619 2581 (:REWRITE DEFAULT-<-1))
 (2880 796 (:REWRITE NFIX-WHEN-NOT-NATP))
 (2687 1185 (:REWRITE NATP-WHEN-GTE-0))
 (2176 2176 (:TYPE-PRESCRIPTION BITP))
 (2034 682 (:REWRITE BITOPS::LOGBITP-WHEN-BITMASKP))
 (1918 1918 (:TYPE-PRESCRIPTION RATIONALP-EXPT-TYPE-PRESCRIPTION))
 (1918 1918 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NON-ZERO-BASE))
 (1440 1440 (:TYPE-PRESCRIPTION BITMASKP$INLINE))
 (1312 274 (:REWRITE UNSIGNED-BYTE-P-OF-LOGTAIL))
 (1297 1185 (:REWRITE NATP-WHEN-INTEGERP))
 (1242 418 (:REWRITE BITOPS::LOGBITP-NONZERO-OF-BIT))
 (1082 576 (:REWRITE DEFAULT-+-2))
 (862 576 (:REWRITE DEFAULT-+-1))
 (700 5 (:LINEAR BITOPS::LOGIOR->=-0-LINEAR))
 (700 5 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-1))
 (682 682 (:REWRITE BITOPS::LOGBITP-OF-NEGATIVE-CONST))
 (682 682 (:REWRITE BITOPS::LOGBITP-OF-MASK))
 (682 682 (:REWRITE BITOPS::LOGBITP-OF-CONST-SPLIT))
 (682 682 (:REWRITE BITOPS::LOGBITP-OF-CONST))
 (572 572 (:REWRITE SV::LOGHEAD-LEMMA))
 (548 548 (:REWRITE BITOPS::UNSIGNED-BYTE-P-INCR))
 (542 271 (:TYPE-PRESCRIPTION BITOPS::LOGNOT-NEGP))
 (542 271 (:TYPE-PRESCRIPTION BITOPS::LOGNOT-NATP))
 (440 5 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-2))
 (390 15 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-2))
 (320 320 (:TYPE-PRESCRIPTION BITOPS::LOGAND-NATP-TYPE-1))
 (291 291 (:TYPE-PRESCRIPTION NEGP))
 (290 15 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 2))
 (250 80 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (250 80 (:REWRITE IFIX-WHEN-INTEGERP))
 (180 20 (:REWRITE BITOPS::LOGNOT-<-CONST))
 (180 15 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-1))
 (180 15 (:LINEAR BITOPS::LOGAND-<-0-LINEAR))
 (166 166 (:REWRITE INEQUALITY-WITH-NFIX-HYP-1))
 (152 152 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (136 136 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (136 68 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (136 68 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (120 20 (:REWRITE IFIX-NEGATIVE-TO-NEGP))
 (104 52 (:REWRITE BFIX-WHEN-NOT-BITP))
 (94 94 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (92 46 (:REWRITE DEFAULT-UNARY-MINUS))
 (90 90 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (80 15 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 1))
 (66 22 (:REWRITE BITOPS::LOGAND-WITH-NEGATED-BITMASK))
 (66 22 (:REWRITE BITOPS::LOGAND-WITH-BITMASK))
 (60 20 (:REWRITE NEGP-WHEN-LESS-THAN-0))
 (54 54 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (50 50 (:REWRITE FOLD-CONSTS-IN-+))
 (48 48 (:REWRITE INEQUALITY-WITH-NFIX-HYP-2))
 (48 48 (:REWRITE BITOPS::B-AND-EQUAL-1-IN-HYP))
 (20 20 (:REWRITE NEGP-WHEN-INTEGERP))
 (12 12 (:REWRITE BITOPS::B-IOR-EQUAL-1-IN-CONCL))
 )
(SV::LOGHEAD-LOGAPP-VS-ASH-LEMMA
 (428 4 (:REWRITE LOGHEAD-IDENTITY))
 (289 17 (:DEFINITION UNSIGNED-BYTE-P))
 (254 13 (:DEFINITION INTEGER-RANGE-P))
 (209 12 (:REWRITE UNSIGNED-BYTE-P-PLUS))
 (206 4 (:REWRITE UNSIGNED-BYTE-P-OF-ASH))
 (72 46 (:REWRITE DEFAULT-<-2))
 (52 46 (:REWRITE DEFAULT-<-1))
 (42 42 (:TYPE-PRESCRIPTION RATIONALP-EXPT-TYPE-PRESCRIPTION))
 (42 42 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NON-ZERO-BASE))
 (36 6 (:REWRITE IFIX-NEGATIVE-TO-NEGP))
 (33 3 (:REWRITE BITOPS::LOGAPP-SIGN))
 (33 3 (:REWRITE BITOPS::ASH-<-0))
 (33 2 (:REWRITE BITOPS::LOGHEAD-OF-LOGAPP-2))
 (30 3 (:REWRITE COMMUTATIVITY-2-OF-+))
 (23 2 (:REWRITE BITOPS::LOGHEAD-OF-LOGAPP-1))
 (22 21 (:REWRITE DEFAULT-+-1))
 (21 21 (:TYPE-PRESCRIPTION LOGAPP-TYPE))
 (21 21 (:TYPE-PRESCRIPTION BITOPS::ASH-NATP-TYPE))
 (21 21 (:REWRITE DEFAULT-+-2))
 (19 19 (:TYPE-PRESCRIPTION NFIX))
 (18 6 (:REWRITE NEGP-WHEN-LESS-THAN-0))
 (17 17 (:TYPE-PRESCRIPTION UNSIGNED-BYTE-P))
 (17 17 (:REWRITE BITOPS::UNSIGNED-BYTE-P-INCR))
 (16 10 (:REWRITE NFIX-WHEN-NATP))
 (16 8 (:REWRITE FOLD-CONSTS-IN-+))
 (15 15 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (15 4 (:REWRITE UNSIGNED-BYTE-P-OF-LOGAPP))
 (14 10 (:REWRITE NFIX-WHEN-NOT-NATP))
 (13 2 (:REWRITE COMMUTATIVITY-OF-+))
 (12 3 (:REWRITE DISTRIBUTIVITY-OF-MINUS-OVER-+))
 (12 2 (:REWRITE RIGHT-SHIFT-TO-LOGTAIL))
 (9 9 (:TYPE-PRESCRIPTION ASH))
 (8 8 (:REWRITE SV::LOGHEAD-LEMMA))
 (8 2 (:REWRITE ASH-0))
 (7 7 (:REWRITE DEFAULT-UNARY-MINUS))
 (6 6 (:TYPE-PRESCRIPTION NEGP))
 (6 6 (:REWRITE NEGP-WHEN-INTEGERP))
 (6 2 (:TYPE-PRESCRIPTION BITOPS::LOGCONS-POSP-1))
 (5 3 (:REWRITE ZP-WHEN-GT-0))
 (4 4 (:TYPE-PRESCRIPTION ZIP))
 (4 4 (:TYPE-PRESCRIPTION NATP))
 (4 2 (:REWRITE UNICITY-OF-0))
 (4 2 (:REWRITE NATP-WHEN-GTE-0))
 (3 3 (:REWRITE ZP-WHEN-INTEGERP))
 (3 3 (:REWRITE ZP-OPEN))
 (2 2 (:TYPE-PRESCRIPTION POSP))
 (2 2 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (2 2 (:REWRITE ZIP-OPEN))
 (2 2 (:REWRITE NATP-WHEN-INTEGERP))
 (2 2 (:REWRITE INEQUALITY-WITH-NFIX-HYP-1))
 (2 2 (:DEFINITION FIX))
 (1 1 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(SV::PART-SELECT-EQUAL-WHEN-4VEC-<<=-AND-2VEC-P
 (1874 28 (:REWRITE LOGHEAD-IDENTITY))
 (1778 79 (:DEFINITION UNSIGNED-BYTE-P))
 (1664 74 (:DEFINITION INTEGER-RANGE-P))
 (1105 13 (:REWRITE UNSIGNED-BYTE-P-OF-LOGTAIL))
 (810 36 (:REWRITE UNSIGNED-BYTE-P-PLUS))
 (764 4 (:LINEAR BITOPS::LOGIOR->=-0-LINEAR))
 (764 4 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-1))
 (533 280 (:REWRITE DEFAULT-<-1))
 (497 202 (:TYPE-PRESCRIPTION BITOPS::LOGNOT-NATP))
 (492 157 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (444 280 (:REWRITE DEFAULT-<-2))
 (424 12 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-2))
 (380 20 (:REWRITE LOGTAIL-IDENTITY))
 (352 4 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-2))
 (345 5 (:REWRITE UNSIGNED-BYTE-P-OF-ASH))
 (320 12 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 2))
 (286 286 (:TYPE-PRESCRIPTION RATIONALP-EXPT-TYPE-PRESCRIPTION))
 (286 286 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NON-ZERO-BASE))
 (268 268 (:TYPE-PRESCRIPTION BITOPS::LOGAND-NATP-TYPE-1))
 (191 191 (:TYPE-PRESCRIPTION NEGP))
 (176 16 (:REWRITE BITOPS::LOGNOT-<-CONST))
 (144 12 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-1))
 (144 12 (:LINEAR BITOPS::LOGAND-<-0-LINEAR))
 (96 12 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 1))
 (79 79 (:REWRITE BITOPS::UNSIGNED-BYTE-P-INCR))
 (60 60 (:REWRITE SV::LOGHEAD-LEMMA))
 (55 5 (:REWRITE BITOPS::LOGAPP-SIGN))
 (55 5 (:REWRITE BITOPS::ASH-<-0))
 (55 5 (:REWRITE ASH-0))
 (54 18 (:REWRITE COMMUTATIVITY-OF-+))
 (54 7 (:REWRITE BITOPS::LOGHEAD-OF-LOGAPP-2))
 (44 44 (:TYPE-PRESCRIPTION BITMASKP$INLINE))
 (39 39 (:REWRITE NFIX-WHEN-NOT-NATP))
 (39 39 (:REWRITE DEFAULT-UNARY-MINUS))
 (36 36 (:REWRITE DEFAULT-+-2))
 (36 36 (:REWRITE DEFAULT-+-1))
 (35 5 (:REWRITE ZIP-OPEN))
 (33 11 (:REWRITE BITOPS::LOGAND-WITH-NEGATED-BITMASK))
 (33 11 (:REWRITE BITOPS::LOGAND-WITH-BITMASK))
 (33 5 (:REWRITE UNSIGNED-BYTE-P-OF-LOGAPP))
 (8 8 (:REWRITE SV::4VEC-P-WHEN-MEMBER-EQUAL-OF-4VECLIST-P))
 (8 2 (:REWRITE ZP-WHEN-GT-0))
 (5 5 (:TYPE-PRESCRIPTION ZIP))
 (4 4 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (3 3 (:REWRITE-QUOTED-CONSTANT SV::MAYBE-4VEC-FIX-UNDER-MAYBE-4VEC-EQUIV))
 (3 3 (:REWRITE-QUOTED-CONSTANT SV::4VEC-FIX-UNDER-4VEC-EQUIV))
 )
(SV::PART-SELECT-EQUAL-WHEN-4VEC-<<=-AND-INTEGERP
 (38 1 (:REWRITE SV::4VEC-<<=-2VEC))
 (17 3 (:REWRITE INTEGERP-OF-CAR-WHEN-INTEGER-LISTP))
 (17 2 (:REWRITE IFIX-WHEN-INTEGERP))
 (11 1 (:DEFINITION INTEGER-LISTP))
 (8 8 (:TYPE-PRESCRIPTION INTEGER-LISTP))
 (8 8 (:TYPE-PRESCRIPTION IFIX))
 (6 2 (:REWRITE SV::4VEC->LOWER-WHEN-2VEC-P))
 (4 2 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (2 2 (:REWRITE INTEGER-LISTP-WHEN-NOT-CONSP))
 (2 2 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 1 (:REWRITE INTEGER-LISTP-OF-CDR-WHEN-INTEGER-LISTP))
 )
(SV::PART-SELECT-OF-SVEX-ENV-LOOKUP-WHEN-INTEGERP-AND-<<=
 (13 1 (:REWRITE SV::4VEC-<<=-2VEC))
 (11 1 (:DEFINITION SV::2VEC-P$INLINE))
 (5 5 (:TYPE-PRESCRIPTION SV::INTEGERP-OF-4VEC->UPPER))
 (3 3 (:TYPE-PRESCRIPTION SV::INTEGERP-OF-4VEC->LOWER))
 (2 2 (:TYPE-PRESCRIPTION SV::2VEC-P$INLINE))
 (2 1 (:REWRITE SV::4VEC->LOWER-WHEN-2VEC-P))
 )
(SV::LOGHEAD-LOGTAIL-LEMMA
 (2705 467 (:REWRITE BITOPS::LOGBITP-WHEN-BIT))
 (1908 288 (:REWRITE RIGHT-SHIFT-TO-LOGTAIL))
 (1560 1560 (:TYPE-PRESCRIPTION BITP))
 (1534 872 (:REWRITE DEFAULT-UNARY-MINUS))
 (1391 467 (:REWRITE BITOPS::LOGBITP-WHEN-BITMASKP))
 (1080 288 (:REWRITE ASH-0))
 (1080 108 (:REWRITE IFIX-POSITIVE-TO-NON-ZP))
 (1012 1012 (:TYPE-PRESCRIPTION BITMASKP$INLINE))
 (986 84 (:REWRITE BITOPS::LOGBITP-OF-ASH-IN-RANGE))
 (959 323 (:REWRITE BITOPS::LOGBITP-NONZERO-OF-BIT))
 (914 630 (:REWRITE DEFAULT-<-1))
 (896 284 (:REWRITE NFIX-WHEN-NOT-NATP))
 (894 482 (:REWRITE DEFAULT-+-2))
 (852 682 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (852 682 (:REWRITE IFIX-WHEN-INTEGERP))
 (836 630 (:REWRITE DEFAULT-<-2))
 (768 482 (:REWRITE DEFAULT-+-1))
 (732 236 (:REWRITE NATP-WHEN-GTE-0))
 (700 5 (:LINEAR BITOPS::LOGIOR->=-0-LINEAR))
 (700 5 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-1))
 (542 271 (:TYPE-PRESCRIPTION BITOPS::LOGNOT-NEGP))
 (542 271 (:TYPE-PRESCRIPTION BITOPS::LOGNOT-NATP))
 (467 467 (:REWRITE BITOPS::LOGBITP-OF-NEGATIVE-CONST))
 (467 467 (:REWRITE BITOPS::LOGBITP-OF-MASK))
 (467 467 (:REWRITE BITOPS::LOGBITP-OF-CONST-SPLIT))
 (467 467 (:REWRITE BITOPS::LOGBITP-OF-CONST))
 (460 460 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (440 5 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-2))
 (432 108 (:REWRITE ZP-OPEN))
 (412 236 (:REWRITE NATP-WHEN-INTEGERP))
 (390 15 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-2))
 (324 108 (:REWRITE ZP-WHEN-GT-0))
 (320 320 (:TYPE-PRESCRIPTION BITOPS::LOGAND-NATP-TYPE-1))
 (291 291 (:TYPE-PRESCRIPTION NEGP))
 (290 15 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 2))
 (180 20 (:REWRITE BITOPS::LOGNOT-<-CONST))
 (180 15 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-1))
 (180 15 (:LINEAR BITOPS::LOGAND-<-0-LINEAR))
 (120 20 (:REWRITE IFIX-NEGATIVE-TO-NEGP))
 (108 108 (:REWRITE ZP-WHEN-INTEGERP))
 (100 100 (:REWRITE INEQUALITY-WITH-NFIX-HYP-2))
 (96 96 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (96 48 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (96 48 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (80 40 (:REWRITE BFIX-WHEN-NOT-BITP))
 (80 15 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 1))
 (71 71 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (67 67 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (66 22 (:REWRITE BITOPS::LOGAND-WITH-NEGATED-BITMASK))
 (66 22 (:REWRITE BITOPS::LOGAND-WITH-BITMASK))
 (60 20 (:REWRITE NEGP-WHEN-LESS-THAN-0))
 (50 50 (:REWRITE FOLD-CONSTS-IN-+))
 (42 42 (:REWRITE BITOPS::B-AND-EQUAL-1-IN-HYP))
 (31 31 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (20 20 (:REWRITE NEGP-WHEN-INTEGERP))
 (12 12 (:REWRITE BITOPS::B-IOR-EQUAL-1-IN-CONCL))
 )
(SV::RSH-EQUAL-WHEN-4VEC-<<=-AND-2VEC-P
 (764 4 (:LINEAR BITOPS::LOGIOR->=-0-LINEAR))
 (764 4 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-1))
 (433 170 (:TYPE-PRESCRIPTION BITOPS::LOGNOT-NATP))
 (424 12 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-2))
 (354 85 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (352 4 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-2))
 (320 12 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 2))
 (242 82 (:REWRITE DEFAULT-<-1))
 (236 36 (:REWRITE ASH-0))
 (204 204 (:TYPE-PRESCRIPTION BITOPS::LOGAND-NATP-TYPE-1))
 (176 16 (:REWRITE BITOPS::LOGNOT-<-CONST))
 (159 159 (:TYPE-PRESCRIPTION NEGP))
 (146 36 (:REWRITE RIGHT-SHIFT-TO-LOGTAIL))
 (144 12 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-1))
 (144 12 (:LINEAR BITOPS::LOGAND-<-0-LINEAR))
 (140 20 (:REWRITE ZIP-OPEN))
 (108 82 (:REWRITE DEFAULT-<-2))
 (96 12 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 1))
 (76 38 (:REWRITE DEFAULT-UNARY-MINUS))
 (44 44 (:TYPE-PRESCRIPTION BITMASKP$INLINE))
 (33 11 (:REWRITE BITOPS::LOGAND-WITH-NEGATED-BITMASK))
 (33 11 (:REWRITE BITOPS::LOGAND-WITH-BITMASK))
 (20 20 (:TYPE-PRESCRIPTION ZIP))
 (4 4 (:REWRITE SV::4VEC-P-WHEN-MEMBER-EQUAL-OF-4VECLIST-P))
 (1 1 (:REWRITE-QUOTED-CONSTANT SV::MAYBE-4VEC-FIX-UNDER-MAYBE-4VEC-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT SV::4VEC-FIX-UNDER-4VEC-EQUIV))
 )
(SV::RSH-EQUAL-WHEN-4VEC-<<=-AND-INTEGERP
 (38 1 (:REWRITE SV::4VEC-<<=-2VEC))
 (17 3 (:REWRITE INTEGERP-OF-CAR-WHEN-INTEGER-LISTP))
 (17 2 (:REWRITE IFIX-WHEN-INTEGERP))
 (11 1 (:DEFINITION INTEGER-LISTP))
 (8 8 (:TYPE-PRESCRIPTION INTEGER-LISTP))
 (8 8 (:TYPE-PRESCRIPTION IFIX))
 (6 2 (:REWRITE SV::4VEC->LOWER-WHEN-2VEC-P))
 (4 2 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (2 2 (:REWRITE INTEGER-LISTP-WHEN-NOT-CONSP))
 (2 2 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 1 (:REWRITE INTEGER-LISTP-OF-CDR-WHEN-INTEGER-LISTP))
 )
(SV::RSH-OF-SVEX-ENV-LOOKUP-WHEN-INTEGERP-AND-<<=
 (13 1 (:REWRITE SV::4VEC-<<=-2VEC))
 (11 1 (:DEFINITION SV::2VEC-P$INLINE))
 (5 5 (:TYPE-PRESCRIPTION SV::INTEGERP-OF-4VEC->UPPER))
 (3 3 (:TYPE-PRESCRIPTION SV::INTEGERP-OF-4VEC->LOWER))
 (2 2 (:TYPE-PRESCRIPTION SV::2VEC-P$INLINE))
 (2 1 (:REWRITE SV::4VEC->LOWER-WHEN-2VEC-P))
 )
