(SV::SVEX-ALIST-EVAL-WHEN-EQUIV-COMPOSE
 (17 2 (:REWRITE SV::SVEX-ALIST-EVAL-OF-APPEND-WHEN-SUBSETP-FIRST))
 (12 2 (:DEFINITION BINARY-APPEND))
 (9 9 (:TYPE-PRESCRIPTION SV::SVEX-ALIST-EVAL))
 (7 4 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (5 1 (:REWRITE SV::SVEX-ENV-FIX-WHEN-SVEX-ENV-P))
 (4 1 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (3 3 (:TYPE-PRESCRIPTION SV::SVEX-ALIST-KEYS))
 (2 2 (:TYPE-PRESCRIPTION SUBSETP-EQUAL))
 (2 2 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE DEFAULT-CAR))
 (1 1 (:TYPE-PRESCRIPTION SV::SVEX-ENV-P))
 (1 1 (:REWRITE SV::SVEX-ENV-P-OF-SVEX-ALIST-EVAL))
 (1 1 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (1 1 (:REWRITE SUBSETP-TRANS2))
 (1 1 (:REWRITE SUBSETP-TRANS))
 (1 1 (:REWRITE SV::ALIST-KEYS-OF-SVEX-ALIST-EVAL))
 )
(SV::EVAL-WHEN-NETEVALCOMP-P
 (21 1 (:REWRITE SV::SVEX-ALIST-EVAL-OF-APPEND-WHEN-SUBSETP-FIRST))
 (12 2 (:DEFINITION BINARY-APPEND))
 (9 9 (:TYPE-PRESCRIPTION SV::SVARLIST-X-ENV))
 (7 4 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (6 1 (:REWRITE SV::ALIST-KEYS-OF-SVARLIST-X-ENV))
 (5 5 (:TYPE-PRESCRIPTION SV::SVEX-ALIST-KEYS))
 (5 1 (:REWRITE SV::SVEX-ENV-FIX-WHEN-SVEX-ENV-P))
 (5 1 (:REWRITE SV::SVARLIST-FIX-WHEN-SVARLIST-P))
 (4 1 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (2 2 (:TYPE-PRESCRIPTION SUBSETP-EQUAL))
 (2 2 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 1 (:REWRITE SV::NETEVAL-ORDERING-COMPILE-WHEN-ATOM-FIX))
 (1 1 (:TYPE-PRESCRIPTION SV::SVEX-ENV-P))
 (1 1 (:TYPE-PRESCRIPTION SV::SVARLIST-P))
 (1 1 (:TYPE-PRESCRIPTION SV::NETEVAL-ORDERING-FIX$INLINE))
 (1 1 (:REWRITE SV::SVEX-ENV-P-OF-SVARLIST-X-ENV))
 (1 1 (:REWRITE SV::SVARLIST-P-OF-SVEX-ALIST-KEYS))
 (1 1 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (1 1 (:REWRITE SUBSETP-TRANS2))
 (1 1 (:REWRITE SUBSETP-TRANS))
 )
(SV::EXTRACT-SIMILAR-TO-NIL-IMPLIES-LOOKUP
 (6 1 (:REWRITE SV::SVARLIST-FIX-WHEN-SVARLIST-P))
 (5 1 (:REWRITE SV::SVAR-FIX-WHEN-SVAR-P))
 (2 2 (:TYPE-PRESCRIPTION SV::SVARLIST-P))
 (2 2 (:TYPE-PRESCRIPTION SV::SVAR-P))
 (2 2 (:REWRITE SV::SVARLIST-P-WHEN-SUBSETP-EQUAL))
 (2 2 (:REWRITE SV::SVAR-P-WHEN-MEMBER-EQUAL-OF-SVARLIST-P))
 (1 1 (:REWRITE-QUOTED-CONSTANT SV::SVEX-ENV-FIX-UNDER-SVEX-ENV-EQUIV))
 (1 1 (:REWRITE SV::SVARLIST-P-WHEN-NOT-CONSP))
 (1 1 (:REWRITE SUBSETP-TRANS2))
 (1 1 (:REWRITE SUBSETP-TRANS))
 (1 1 (:REWRITE SUBSETP-MEMBER . 4))
 (1 1 (:REWRITE SUBSETP-MEMBER . 3))
 (1 1 (:REWRITE SUBSETP-MEMBER . 2))
 (1 1 (:REWRITE INTERSECTP-MEMBER . 3))
 (1 1 (:REWRITE INTERSECTP-MEMBER . 2))
 )
(SV::APPEND-NETEVAL-ORDERING-EVAL-ENV-IS-APPEND-EXTRACT
 (349 30 (:REWRITE SV::SVEX-ENV-EXTRACT-WHEN-ALIST-KEYS-EQUAL))
 (264 44 (:DEFINITION BINARY-APPEND))
 (202 88 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (150 38 (:REWRITE SV::SVEX-ENV-FIX-WHEN-SVEX-ENV-P))
 (145 145 (:TYPE-PRESCRIPTION SV::NETEVAL-ORDERING-EVAL))
 (108 108 (:TYPE-PRESCRIPTION ALIST-KEYS))
 (101 101 (:TYPE-PRESCRIPTION SV::SVEX-ENV-EXTRACT))
 (80 80 (:REWRITE DEFAULT-CAR))
 (80 11 (:REWRITE SV::SVEX-ENV-EXTRACT-KEYS-UNDER-SVEX-ENVS-EQUIVALENT))
 (68 16 (:REWRITE SV::SVAR-FIX-WHEN-SVAR-P))
 (65 65 (:REWRITE DEFAULT-CDR))
 (44 44 (:REWRITE SV::SVAR-P-WHEN-MEMBER-EQUAL-OF-SVARLIST-P))
 (33 3 (:REWRITE SV::NETEVAL-SIGORDERING-FIX-WHEN-NETEVAL-SIGORDERING-P))
 (26 26 (:TYPE-PRESCRIPTION SV::SVAR-P))
 (22 22 (:TYPE-PRESCRIPTION SET-EQUIV))
 (22 22 (:REWRITE SV::RETURN-TYPE-OF-NETEVAL-ORDERING-EVAL.NETEVAL))
 (18 3 (:REWRITE SV::NETEVAL-SIGORDERING-P-OF-CDR-OF-HONS-ASSOC-EQUAL-WHEN-NETEVAL-ORDERING-P))
 (18 3 (:REWRITE SV::NETEVAL-ORDERING-FIX-WHEN-NETEVAL-ORDERING-P))
 (12 12 (:REWRITE SV::NETEVAL-ORDERING-P-WHEN-SUBSETP-EQUAL))
 (8 8 (:REWRITE-QUOTED-CONSTANT SV::SVEX-ENV-FIX-UNDER-SVEX-ENV-EQUIV))
 (8 8 (:REWRITE SV::SVEX-ENV-P-WHEN-NOT-CONSP))
 (8 2 (:REWRITE MEMBER-WHEN-ATOM))
 (6 6 (:TYPE-PRESCRIPTION SV::NETEVAL-SIGORDERING-P))
 (6 6 (:REWRITE SUBSETP-MEMBER . 2))
 (6 6 (:REWRITE SUBSETP-MEMBER . 1))
 (6 6 (:REWRITE SV::REWRITE-MEMBER-OF-APPEND-UNDER-SET-EQUIV))
 (6 6 (:REWRITE SV::NETEVAL-ORDERING-P-WHEN-NOT-CONSP))
 (2 2 (:REWRITE SUBSETP-MEMBER . 4))
 (2 2 (:REWRITE SUBSETP-MEMBER . 3))
 (2 2 (:REWRITE INTERSECTP-MEMBER . 3))
 (2 2 (:REWRITE INTERSECTP-MEMBER . 2))
 )
(SV::EVAL-LOOKUP-OF-EXTRACT
 (54 2 (:REWRITE SV::SVEX-EVAL-OF-APPEND-WHEN-SUBSETP-FIRST))
 (21 3 (:REWRITE SV::SVEX-ENV-EXTRACT-WHEN-ALIST-KEYS-EQUAL))
 (18 3 (:DEFINITION BINARY-APPEND))
 (16 2 (:REWRITE INTERSECTP-EQUAL-COMMUTE))
 (14 14 (:TYPE-PRESCRIPTION BINARY-APPEND))
 (14 2 (:REWRITE SV::SVEX-ENV-EXTRACT-KEYS-UNDER-SVEX-ENVS-EQUIVALENT))
 (12 6 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (10 2 (:REWRITE SV::SVEX-EVAL-WHEN-FNCALL))
 (8 8 (:TYPE-PRESCRIPTION ALIST-KEYS))
 (8 4 (:REWRITE SV::SVEX-VARS-WHEN-VAR))
 (8 4 (:REWRITE SV::SVEX-VARS-WHEN-QUOTE))
 (8 4 (:REWRITE SV::SVEX-VARS-WHEN-CALL))
 (8 2 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (8 2 (:DEFINITION EQ))
 (6 3 (:REWRITE INTERSECTP-EQUAL-NON-CONS))
 (5 5 (:REWRITE INTERSECTP-MEMBER . 1))
 (4 4 (:TYPE-PRESCRIPTION SUBSETP-EQUAL))
 (4 4 (:TYPE-PRESCRIPTION SET-EQUIV))
 (3 3 (:REWRITE SV::SVEX-UNIFY-CORRECT))
 (3 3 (:REWRITE DEFAULT-CDR))
 (3 3 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE SV::SVEX-EVAL-OF-QUOTED))
 (2 2 (:REWRITE SV::SVEX-ENV-P-WHEN-NOT-CONSP))
 (2 2 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (2 2 (:REWRITE SUBSETP-TRANS2))
 (2 2 (:REWRITE SUBSETP-TRANS))
 )
(SV::4VEC-RSH-WHEN-ZP
 (27 5 (:REWRITE SV::4VEC->LOWER-WHEN-2VEC-P))
 (11 1 (:REWRITE SV::2VEC-P$INLINE-OF-4VEC-FIX-X))
 (10 1 (:REWRITE SV::4VEC-FIX-OF-4VEC))
 (8 4 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (6 6 (:REWRITE SV::4VEC-P-WHEN-MEMBER-EQUAL-OF-4VECLIST-P))
 (1 1 (:REWRITE ZIP-OPEN))
 )
(SV::4VEC-<<=-OF-4VEC-RSH
 (1803 7 (:LINEAR BITOPS::LOGIOR->=-0-LINEAR))
 (1803 7 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-1))
 (1660 830 (:REWRITE DEFAULT-UNARY-MINUS))
 (1484 541 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (1320 474 (:REWRITE SV::3VEC-P-IMPLIES-BITS))
 (1120 200 (:REWRITE ASH-0))
 (955 501 (:REWRITE DEFAULT-<-2))
 (924 924 (:REWRITE BITOPS::LOGBITP-OF-NEGATIVE-CONST))
 (924 924 (:REWRITE BITOPS::LOGBITP-OF-MASK))
 (924 924 (:REWRITE BITOPS::LOGBITP-OF-CONST))
 (846 846 (:TYPE-PRESCRIPTION SV::3VEC-P))
 (826 14 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-2))
 (741 501 (:REWRITE DEFAULT-<-1))
 (708 140 (:REWRITE BFIX-WHEN-NOT-1))
 (671 393 (:REWRITE DEFAULT-+-2))
 (666 14 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 2))
 (644 92 (:REWRITE ZIP-OPEN))
 (596 28 (:REWRITE BITOPS::LOGNOT-<-CONST))
 (552 200 (:REWRITE RIGHT-SHIFT-TO-LOGTAIL))
 (541 541 (:TYPE-PRESCRIPTION BITOPS::LOGAND-NATP-TYPE-1))
 (534 393 (:REWRITE DEFAULT-+-1))
 (534 170 (:REWRITE BITOPS::LOGBITP-OF-ASH-IN-RANGE))
 (524 170 (:REWRITE BITOPS::LOGBITP-OF-ASH-OUT-OF-RANGE))
 (480 480 (:TYPE-PRESCRIPTION BITMASKP$INLINE))
 (392 392 (:REWRITE NFIX-WHEN-NOT-NATP))
 (366 126 (:REWRITE BITOPS::LOGAND-WITH-NEGATED-BITMASK))
 (366 126 (:REWRITE BITOPS::LOGAND-WITH-BITMASK))
 (320 14 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-1))
 (320 14 (:LINEAR BITOPS::LOGAND-<-0-LINEAR))
 (280 280 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (280 140 (:REWRITE BFIX-WHEN-NOT-BITP))
 (280 140 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (280 140 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (264 24 (:REWRITE BITOPS::ASH-<-0))
 (240 14 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 1))
 (96 96 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (93 93 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (93 93 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (92 92 (:REWRITE BITOPS::B-IOR-EQUAL-1-IN-CONCL))
 (80 24 (:REWRITE EQUAL-1-OF-BOOL->BIT))
 (58 58 (:REWRITE BITOPS::LOGAND-FOLD-CONSTS))
 (57 19 (:REWRITE NATP-WHEN-GTE-0))
 (55 55 (:REWRITE BITOPS::B-AND-EQUAL-1-IN-HYP))
 (52 52 (:TYPE-PRESCRIPTION NFIX))
 (26 26 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (19 19 (:REWRITE NATP-WHEN-INTEGERP))
 (2 2 (:REWRITE-QUOTED-CONSTANT SV::MAYBE-4VEC-FIX-UNDER-MAYBE-4VEC-EQUIV))
 (2 2 (:REWRITE-QUOTED-CONSTANT SV::4VEC-FIX-UNDER-4VEC-EQUIV))
 )
(SV::4VEC-<<=-OF-4VEC-CONCAT
 (10699 4177 (:REWRITE SV::3VEC-P-IMPLIES-BITS))
 (8871 4467 (:REWRITE NFIX-WHEN-NOT-NATP))
 (7353 7353 (:REWRITE BITOPS::LOGBITP-OF-NEGATIVE-CONST))
 (7353 7353 (:REWRITE BITOPS::LOGBITP-OF-MASK))
 (7353 7353 (:REWRITE BITOPS::LOGBITP-OF-CONST))
 (6522 6522 (:TYPE-PRESCRIPTION SV::3VEC-P))
 (4983 3735 (:REWRITE DEFAULT-<-1))
 (3863 3735 (:REWRITE DEFAULT-<-2))
 (2784 2784 (:TYPE-PRESCRIPTION BITMASKP$INLINE))
 (2508 870 (:REWRITE BITOPS::LOGBITP-OF-LOGAPP-SECOND))
 (2502 870 (:REWRITE BITOPS::LOGBITP-OF-LOGAPP-FIRST))
 (2093 701 (:REWRITE BITOPS::LOGAND-WITH-NEGATED-BITMASK))
 (2093 701 (:REWRITE BITOPS::LOGAND-WITH-BITMASK))
 (2088 1884 (:REWRITE DEFAULT-+-1))
 (2034 1884 (:REWRITE DEFAULT-+-2))
 (1904 1656 (:REWRITE DEFAULT-UNARY-MINUS))
 (1400 1400 (:TYPE-PRESCRIPTION BIT->BOOL$INLINE))
 (1400 700 (:REWRITE BFIX-WHEN-NOT-BIT->BOOL))
 (1400 700 (:REWRITE BFIX-WHEN-BIT->BOOL))
 (1384 692 (:REWRITE BFIX-WHEN-NOT-BITP))
 (1308 8 (:LINEAR BITOPS::LOGIOR->=-0-LINEAR))
 (1308 8 (:LINEAR BITOPS::LOGIOR-<-0-LINEAR-1))
 (1082 62 (:REWRITE LOGTAIL-IDENTITY))
 (944 38 (:DEFINITION UNSIGNED-BYTE-P))
 (823 823 (:TYPE-PRESCRIPTION BITOPS::LOGAND-NATP-TYPE-1))
 (798 38 (:DEFINITION INTEGER-RANGE-P))
 (744 744 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (738 738 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (738 738 (:REWRITE-QUOTED-CONSTANT BFIX-UNDER-BIT-EQUIV))
 (560 16 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-2))
 (463 131 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (432 16 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 2))
 (352 32 (:REWRITE BITOPS::LOGNOT-<-CONST))
 (344 344 (:REWRITE BITOPS::LOGAND-FOLD-CONSTS))
 (264 24 (:REWRITE BITOPS::LOGAPP-SIGN))
 (258 258 (:REWRITE BITOPS::B-IOR-EQUAL-1-IN-CONCL))
 (248 16 (:LINEAR BITOPS::LOGAND->=-0-LINEAR-1))
 (248 16 (:LINEAR BITOPS::LOGAND-<-0-LINEAR))
 (208 208 (:REWRITE BITOPS::B-AND-EQUAL-1-IN-HYP))
 (184 16 (:LINEAR BITOPS::UPPER-BOUND-OF-LOGAND . 1))
 (152 152 (:TYPE-PRESCRIPTION RATIONALP-EXPT-TYPE-PRESCRIPTION))
 (152 152 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NON-ZERO-BASE))
 (105 105 (:REWRITE FOLD-CONSTS-IN-+))
 (54 54 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (38 38 (:REWRITE BITOPS::UNSIGNED-BYTE-P-INCR))
 (20 20 (:REWRITE INEQUALITY-WITH-NFIX-HYP-1))
 )
(SV::SVEX-ENV-EXTRACT-NIL-UNDER-SVEX-ENVS-SIMILAR
 (25 1 (:DEFINITION MEMBER-EQUAL))
 (12 12 (:TYPE-PRESCRIPTION SV::SVARLIST-FIX$INLINE))
 (8 2 (:REWRITE MEMBER-WHEN-ATOM))
 (6 1 (:REWRITE SV::SVARLIST-FIX-WHEN-SVARLIST-P))
 (5 1 (:REWRITE SV::SVEX-ENV-EXTRACT-KEYS-UNDER-SVEX-ENVS-EQUIVALENT))
 (5 1 (:REWRITE SV::SVAR-FIX-WHEN-SVAR-P))
 (4 4 (:REWRITE SV::CONSP-OF-SVARLIST-FIX))
 (4 1 (:REWRITE DEFAULT-CDR))
 (4 1 (:REWRITE DEFAULT-CAR))
 (3 3 (:REWRITE SV::EXTRACT-SIMILAR-TO-NIL-IMPLIES-LOOKUP))
 (3 1 (:REWRITE SET-EQUIV-ASYM))
 (2 2 (:TYPE-PRESCRIPTION SV::SVARLIST-P))
 (2 2 (:TYPE-PRESCRIPTION SV::SVAR-P))
 (2 2 (:TYPE-PRESCRIPTION SV::SVAR-FIX$INLINE))
 (2 2 (:REWRITE-QUOTED-CONSTANT SV::SVEX-ENV-FIX-UNDER-SVEX-ENV-EQUIV))
 (2 2 (:REWRITE SV::SVARLIST-P-WHEN-SUBSETP-EQUAL))
 (2 2 (:REWRITE SV::SVAR-P-WHEN-MEMBER-EQUAL-OF-SVARLIST-P))
 (2 2 (:REWRITE SUBSETP-MEMBER . 4))
 (2 2 (:REWRITE SUBSETP-MEMBER . 3))
 (2 2 (:REWRITE SUBSETP-MEMBER . 2))
 (2 2 (:REWRITE SUBSETP-MEMBER . 1))
 (2 2 (:REWRITE INTERSECTP-MEMBER . 3))
 (2 2 (:REWRITE INTERSECTP-MEMBER . 2))
 (2 1 (:REWRITE SV::SVEX-ENV-EXTRACT-WHEN-ALIST-KEYS-EQUAL))
 (2 1 (:REWRITE SET-EQUIV-OF-NIL))
 (1 1 (:TYPE-PRESCRIPTION SET-EQUIV))
 (1 1 (:TYPE-PRESCRIPTION ALIST-KEYS))
 (1 1 (:REWRITE SV::SVARLIST-P-WHEN-NOT-CONSP))
 (1 1 (:REWRITE SV::REWRITE-MEMBER-OF-APPEND-UNDER-SET-EQUIV))
 (1 1 (:DEFINITION ATOM))
 )
(SV::SVEX-ENV-EXTRACT-CONS-NON-MEMBER
 (529 34 (:REWRITE SV::SVEX-ENV-EXTRACT-WHEN-ALIST-KEYS-EQUAL))
 (523 16 (:DEFINITION MEMBER-EQUAL))
 (322 46 (:REWRITE SV::SVAR-FIX-WHEN-SVAR-P))
 (321 17 (:REWRITE SV::SVEX-ENV-FIX-OF-ACONS))
 (180 22 (:REWRITE SV::SVAR-P-OF-CAR-WHEN-SVARLIST-P))
 (170 17 (:REWRITE SV::4VEC-FIX-OF-4VEC))
 (159 99 (:REWRITE DEFAULT-CAR))
 (154 34 (:REWRITE SUBSETP-MEMBER . 1))
 (144 19 (:REWRITE SV::SVARLIST-FIX-WHEN-SVARLIST-P))
 (136 136 (:TYPE-PRESCRIPTION ALIST-KEYS))
 (119 59 (:REWRITE DEFAULT-CDR))
 (108 108 (:REWRITE SV::SVARLIST-P-WHEN-SUBSETP-EQUAL))
 (106 34 (:REWRITE SV::SVEX-ENV-FIX-WHEN-SVEX-ENV-P))
 (102 82 (:REWRITE SV::SVAR-P-WHEN-MEMBER-EQUAL-OF-SVARLIST-P))
 (100 25 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (85 17 (:REWRITE SV::4VEC-P-WHEN-MAYBE-4VEC-P))
 (78 13 (:REWRITE SV::SVARLIST-P-OF-CDR-WHEN-SVARLIST-P))
 (70 25 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (60 60 (:TYPE-PRESCRIPTION SV::SVAR-P))
 (55 54 (:REWRITE SV::SVARLIST-P-WHEN-NOT-CONSP))
 (51 51 (:TYPE-PRESCRIPTION SV::4VEC-P))
 (50 5 (:REWRITE SUBSETP-IMPLIES-SUBSETP-CDR))
 (36 36 (:REWRITE SUBSETP-MEMBER . 4))
 (36 36 (:REWRITE INTERSECTP-MEMBER . 3))
 (36 36 (:REWRITE INTERSECTP-MEMBER . 2))
 (34 34 (:TYPE-PRESCRIPTION SV::MAYBE-4VEC-P))
 (34 34 (:REWRITE SUBSETP-MEMBER . 2))
 (34 34 (:REWRITE SV::4VEC-P-WHEN-MEMBER-EQUAL-OF-4VECLIST-P))
 (34 17 (:REWRITE SV::MAYBE-4VEC-P-WHEN-4VEC-P))
 (29 29 (:REWRITE SV::EXTRACT-SIMILAR-TO-NIL-IMPLIES-LOOKUP))
 (27 27 (:REWRITE SUBSETP-TRANS2))
 (27 27 (:REWRITE SUBSETP-TRANS))
 (24 24 (:REWRITE SV::SVEX-ENV-P-WHEN-NOT-CONSP))
 (16 16 (:REWRITE SV::REWRITE-MEMBER-OF-APPEND-UNDER-SET-EQUIV))
 (7 7 (:REWRITE SV::LOOKUP-WHEN-SVEX-ENVS-AGREE))
 (5 5 (:REWRITE SV::SVARLIST-P-OF-SVARLIST-FIX))
 )
(SV::SVEX-ENV-<<=-EXTRACT-CONS-LEMMA
 (434 11 (:REWRITE SV::SVEX-ENV-EXTRACT-CONS-NON-MEMBER))
 (302 11 (:DEFINITION MEMBER-EQUAL))
 (231 18 (:REWRITE SV::SVEX-ENV-EXTRACT-WHEN-ALIST-KEYS-EQUAL))
 (219 13 (:REWRITE SV::SVEX-ENV-FIX-OF-ACONS))
 (137 137 (:TYPE-PRESCRIPTION SV::SVARLIST-FIX$INLINE))
 (130 13 (:REWRITE SV::4VEC-FIX-OF-4VEC))
 (127 11 (:REWRITE SV::SVEX-ENV-EXTRACT-KEYS-UNDER-SVEX-ENVS-EQUIVALENT))
 (103 10 (:REWRITE SV::4VEC-<<=-2VEC))
 (82 22 (:REWRITE MEMBER-WHEN-ATOM))
 (81 9 (:DEFINITION SV::2VEC-P$INLINE))
 (67 25 (:REWRITE SV::SVEX-ENV-FIX-WHEN-SVEX-ENV-P))
 (65 13 (:REWRITE SV::4VEC-P-WHEN-MAYBE-4VEC-P))
 (60 60 (:TYPE-PRESCRIPTION ALIST-KEYS))
 (58 13 (:REWRITE SV::SVARLIST-FIX-WHEN-SVARLIST-P))
 (44 11 (:REWRITE DEFAULT-CDR))
 (44 11 (:REWRITE DEFAULT-CAR))
 (42 42 (:REWRITE SV::CONSP-OF-SVARLIST-FIX))
 (39 39 (:TYPE-PRESCRIPTION SV::4VEC-P))
 (36 36 (:REWRITE SV::SVAR-P-WHEN-MEMBER-EQUAL-OF-SVARLIST-P))
 (35 35 (:TYPE-PRESCRIPTION SV::INTEGERP-OF-4VEC->UPPER))
 (28 28 (:TYPE-PRESCRIPTION SV::SVEX-ENV-P))
 (26 26 (:TYPE-PRESCRIPTION SV::MAYBE-4VEC-P))
 (26 26 (:REWRITE SV::4VEC-P-WHEN-MEMBER-EQUAL-OF-4VECLIST-P))
 (26 13 (:REWRITE SV::MAYBE-4VEC-P-WHEN-4VEC-P))
 (26 6 (:REWRITE SV::SVAR-FIX-WHEN-SVAR-P))
 (24 24 (:REWRITE SUBSETP-MEMBER . 4))
 (24 24 (:REWRITE INTERSECTP-MEMBER . 3))
 (24 24 (:REWRITE INTERSECTP-MEMBER . 2))
 (23 23 (:TYPE-PRESCRIPTION SV::SVAR-P))
 (23 23 (:REWRITE SUBSETP-MEMBER . 2))
 (21 21 (:TYPE-PRESCRIPTION SV::INTEGERP-OF-4VEC->LOWER))
 (21 21 (:TYPE-PRESCRIPTION SV::2VEC-P$INLINE))
 (20 20 (:TYPE-PRESCRIPTION SET-EQUIV))
 (18 18 (:TYPE-PRESCRIPTION SV::SVARLIST-P))
 (18 18 (:REWRITE SV::SVARLIST-P-WHEN-SUBSETP-EQUAL))
 (17 17 (:REWRITE SV::EXTRACT-SIMILAR-TO-NIL-IMPLIES-LOOKUP))
 (14 14 (:REWRITE SV::SVEX-ENV-P-WHEN-NOT-CONSP))
 (14 7 (:REWRITE SV::4VEC->LOWER-WHEN-2VEC-P))
 (11 11 (:REWRITE SV::REWRITE-MEMBER-OF-APPEND-UNDER-SET-EQUIV))
 (9 9 (:REWRITE SV::SVARLIST-P-WHEN-NOT-CONSP))
 (9 9 (:REWRITE SV::SVARLIST-FIX-UNDER-IFF))
 (3 1 (:REWRITE SV::SVEX-ENV-<<=-NECC))
 (2 2 (:REWRITE-QUOTED-CONSTANT SV::MAYBE-4VEC-FIX-UNDER-MAYBE-4VEC-EQUIV))
 (2 2 (:REWRITE-QUOTED-CONSTANT SV::4VEC-FIX-UNDER-4VEC-EQUIV))
 (2 2 (:REWRITE SUBSETP-TRANS2))
 (2 2 (:REWRITE SUBSETP-TRANS))
 )
(SV::FLAG-LEMMA-FOR-NETEVAL-ORDERING-EVAL-<<=-FIXPOINT
 (318 27 (:REWRITE SV::SVEX-ENV-EXTRACT-WHEN-ALIST-KEYS-EQUAL))
 (289 17 (:DEFINITION MEMBER-EQUAL))
 (262 36 (:REWRITE SUBSETP-CAR-MEMBER))
 (207 27 (:REWRITE SV::SVEX-ENV-EXTRACT-KEYS-UNDER-SVEX-ENVS-EQUIVALENT))
 (185 54 (:REWRITE SV::SVEX-ENV-FIX-WHEN-SVEX-ENV-P))
 (108 108 (:TYPE-PRESCRIPTION ALIST-KEYS))
 (106 9 (:REWRITE SV::4VEC-<<=-2VEC))
 (85 34 (:REWRITE NO-DUPLICATESP-EQUAL-NON-CONS))
 (72 72 (:TYPE-PRESCRIPTION SUBSETP-EQUAL))
 (65 59 (:REWRITE DEFAULT-CDR))
 (59 19 (:REWRITE SV::NETEVAL-ORDERING-FIX-WHEN-NETEVAL-ORDERING-P))
 (54 54 (:TYPE-PRESCRIPTION SET-EQUIV))
 (52 46 (:REWRITE DEFAULT-CAR))
 (48 7 (:REWRITE SV::4VEC-RSH-WHEN-ZP))
 (42 36 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (42 36 (:REWRITE MEMBER-WHEN-ATOM))
 (40 40 (:TYPE-PRESCRIPTION SV::NETEVAL-ORDERING-EVAL))
 (40 40 (:TYPE-PRESCRIPTION SV::INTEGERP-OF-4VEC->UPPER))
 (40 36 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (37 37 (:REWRITE SUBSETP-MEMBER . 2))
 (37 37 (:REWRITE SUBSETP-MEMBER . 1))
 (36 36 (:REWRITE SUBSETP-TRANS2))
 (36 36 (:REWRITE SUBSETP-TRANS))
 (36 36 (:REWRITE SUBSETP-MEMBER . 4))
 (36 36 (:REWRITE SUBSETP-MEMBER . 3))
 (36 36 (:REWRITE INTERSECTP-MEMBER . 3))
 (36 36 (:REWRITE INTERSECTP-MEMBER . 2))
 (34 34 (:REWRITE FN-CHECK-DEF-FORMALS))
 (30 2 (:REWRITE ZIP-OPEN))
 (26 2 (:REWRITE SV::SVEX-ENV-FIX-OF-ACONS))
 (24 24 (:TYPE-PRESCRIPTION SV::INTEGERP-OF-4VEC->LOWER))
 (22 11 (:REWRITE IFIX-WHEN-NOT-INTEGERP))
 (22 8 (:REWRITE NFIX-WHEN-NOT-NATP))
 (20 20 (:REWRITE SV::REWRITE-MEMBER-OF-APPEND-UNDER-SET-EQUIV))
 (20 20 (:REWRITE SV::RETURN-TYPE-OF-NETEVAL-ORDERING-EVAL.NETEVAL))
 (19 19 (:REWRITE-QUOTED-CONSTANT SV::SVEX-ENV-FIX-UNDER-SVEX-ENV-EQUIV))
 (17 17 (:REWRITE SV::SVEX-ENV-P-WHEN-NOT-CONSP))
 (17 17 (:REWRITE SV::SVEX-ALIST-WIDTH-WHEN-WIDTH-LIMITED-P))
 (16 16 (:REWRITE SV::NETEVAL-ORDERING-P-WHEN-SUBSETP-EQUAL))
 (16 8 (:REWRITE SV::CONSP-CAR-OF-NETEVAL-ORDERING-FIX))
 (16 8 (:REWRITE SV::4VEC->LOWER-WHEN-2VEC-P))
 (16 4 (:REWRITE SV::SVAR-P-OF-CAAR-WHEN-SVAR-WIDTH-MAP-P))
 (14 2 (:REWRITE NFIX-EQUAL-TO-ZERO))
 (12 6 (:REWRITE NATP-WHEN-GTE-0))
 (8 8 (:REWRITE SV::NETEVAL-ORDERING-P-WHEN-NOT-CONSP))
 (8 4 (:REWRITE DEFAULT-+-2))
 (8 4 (:REWRITE DEFAULT-+-1))
 (6 6 (:REWRITE NATP-WHEN-INTEGERP))
 (6 2 (:REWRITE ZP-WHEN-GT-0))
 (6 2 (:REWRITE SV::4VEC-FIX-OF-4VEC))
 (6 1 (:DEFINITION BINARY-APPEND))
 (5 5 (:TYPE-PRESCRIPTION SV::SVEX-ENV-EXTRACT))
 (5 5 (:REWRITE DEFAULT-<-2))
 (5 5 (:REWRITE DEFAULT-<-1))
 (5 2 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (5 1 (:REWRITE SV::SVAR-FIX-WHEN-SVAR-P))
 (4 4 (:REWRITE SV::SVAR-WIDTH-MAP-P-WHEN-SUBSETP-EQUAL))
 (4 4 (:REWRITE SV::SVAR-P-WHEN-MEMBER-EQUAL-OF-SVARLIST-P))
 (3 3 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (3 3 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (2 2 (:TYPE-PRESCRIPTION ZP))
 (2 2 (:TYPE-PRESCRIPTION ZIP))
 (2 2 (:TYPE-PRESCRIPTION SV::SVAR-P))
 (2 2 (:TYPE-PRESCRIPTION SV::4VEC-P))
 (2 2 (:REWRITE ZP-WHEN-INTEGERP))
 (2 2 (:REWRITE ZP-OPEN))
 (2 2 (:REWRITE SV::SVAR-WIDTH-MAP-P-WHEN-NOT-CONSP))
 (2 2 (:REWRITE SV::RETURN-TYPE-OF-NETEVAL-SIGORDERING-EVAL.VAL))
 (2 2 (:REWRITE NFIX-EQUAL-TO-NONZERO-CONST))
 (2 2 (:REWRITE NFIX-EQUAL-TO-NONZERO))
 (1 1 (:REWRITE-QUOTED-CONSTANT SV::MAYBE-4VEC-FIX-UNDER-MAYBE-4VEC-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT SV::4VEC-FIX-UNDER-4VEC-EQUIV))
 (1 1 (:REWRITE SV::SVEX-COMPOSE-LOOKUP-WHEN-NOT-SVEX-LOOKUP))
 )
(SV::NETEVAL-ORDERING-EVAL-<<=-FIXPOINT)
(SV::NETEVAL-SIGORDERING-EVAL-<<=-FIXPOINT)
(SV::NETEVAL-ORDERING-OR-NUL-EVAL-<<=-FIXPOINT)
(SV::SVEX-ALIST-KEYS-WHEN-EQUIV-COMPOSE)
(SV::ALIST-KEYS-OF-NETEVALCOMP-P-WITNESS-WHEN-NETEVALCOMP-P
 (7 2 (:REWRITE SV::NETEVAL-ORDERING-FIX-WHEN-NETEVAL-ORDERING-P))
 (2 2 (:TYPE-PRESCRIPTION SV::NETEVAL-ORDERING-P))
 (2 2 (:REWRITE SV::NETEVAL-ORDERING-P-WHEN-SUBSETP-EQUAL))
 (2 1 (:REWRITE SV::NETEVAL-ORDERING-COMPILE-WHEN-ATOM-FIX))
 (1 1 (:TYPE-PRESCRIPTION SV::NETEVAL-ORDERING-FIX$INLINE))
 (1 1 (:REWRITE SV::NETEVAL-ORDERING-P-WHEN-NOT-CONSP))
 )
(SV::INDUCT-DOWN-NETEVAL-ORDERING
 (12 2 (:REWRITE SV::NETEVAL-ORDERING-FIX-WHEN-NETEVAL-ORDERING-P))
 (4 4 (:REWRITE SV::NETEVAL-ORDERING-P-WHEN-SUBSETP-EQUAL))
 (3 1 (:REWRITE DEFAULT-<-2))
 (3 1 (:REWRITE DEFAULT-<-1))
 (2 2 (:REWRITE SV::NETEVAL-ORDERING-P-WHEN-NOT-CONSP))
 (2 2 (:REWRITE DEFAULT-CDR))
 (1 1 (:TYPE-PRESCRIPTION SV::INDUCT-DOWN-NETEVAL-ORDERING))
 )
(SV::ALIST-KEYS-OF-NETEVAL-ORDERING-EVAL
 (29 24 (:REWRITE DEFAULT-CAR))
 (19 16 (:REWRITE DEFAULT-CDR))
 (10 10 (:REWRITE SV::NETEVAL-ORDERING-P-WHEN-SUBSETP-EQUAL))
 (5 5 (:REWRITE SV::NETEVAL-ORDERING-P-WHEN-NOT-CONSP))
 (3 3 (:TYPE-PRESCRIPTION SV::INDUCT-DOWN-NETEVAL-ORDERING))
 (1 1 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 )
(SV::SVEX-ENVS-SIMILAR-APPEND-EXTRACT-SVARLIST-X-ENV
 (50 2 (:DEFINITION MEMBER-EQUAL))
 (25 5 (:REWRITE SV::SVARLIST-FIX-WHEN-SVARLIST-P))
 (22 22 (:TYPE-PRESCRIPTION SV::SVARLIST-FIX$INLINE))
 (21 5 (:REWRITE SV::SVAR-FIX-WHEN-SVAR-P))
 (18 3 (:DEFINITION BINARY-APPEND))
 (13 4 (:REWRITE MEMBER-WHEN-ATOM))
 (12 6 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (11 5 (:REWRITE DEFAULT-CDR))
 (11 5 (:REWRITE DEFAULT-CAR))
 (9 9 (:REWRITE SV::EXTRACT-SIMILAR-TO-NIL-IMPLIES-LOOKUP))
 (8 8 (:TYPE-PRESCRIPTION SV::SVARLIST-P))
 (8 8 (:TYPE-PRESCRIPTION SV::SVAR-P))
 (8 8 (:REWRITE SV::SVARLIST-P-WHEN-SUBSETP-EQUAL))
 (8 8 (:REWRITE SV::SVAR-P-WHEN-MEMBER-EQUAL-OF-SVARLIST-P))
 (8 2 (:REWRITE SV::SVEX-ENV-EXTRACT-KEYS-UNDER-SVEX-ENVS-EQUIVALENT))
 (7 7 (:TYPE-PRESCRIPTION SV::SVEX-ENV-EXTRACT))
 (7 7 (:REWRITE SUBSETP-MEMBER . 4))
 (7 7 (:REWRITE INTERSECTP-MEMBER . 3))
 (7 7 (:REWRITE INTERSECTP-MEMBER . 2))
 (7 7 (:REWRITE SV::CONSP-OF-SVARLIST-FIX))
 (7 2 (:REWRITE SV::SVEX-ENV-EXTRACT-WHEN-ALIST-KEYS-EQUAL))
 (6 6 (:REWRITE SUBSETP-MEMBER . 2))
 (5 5 (:TYPE-PRESCRIPTION SV::SVARLIST-X-ENV))
 (5 2 (:REWRITE SV::SVEX-ENV-FIX-WHEN-SVEX-ENV-P))
 (4 4 (:TYPE-PRESCRIPTION SV::SVAR-FIX$INLINE))
 (4 4 (:TYPE-PRESCRIPTION ALIST-KEYS))
 (4 4 (:REWRITE SV::SVARLIST-P-WHEN-NOT-CONSP))
 (3 3 (:REWRITE SUBSETP-TRANS2))
 (3 3 (:REWRITE SUBSETP-TRANS))
 (2 2 (:TYPE-PRESCRIPTION SV::SVEX-ENV-P))
 (2 2 (:TYPE-PRESCRIPTION SET-EQUIV))
 (2 2 (:REWRITE SV::REWRITE-MEMBER-OF-APPEND-UNDER-SET-EQUIV))
 (1 1 (:REWRITE SV::SVEX-ENV-P-WHEN-NOT-CONSP))
 )
(SV::SVEX-ALIST-EVAL-FIXPOINT-STEP-OF-APPEND-SVARLIST-X-ENV)
(SV::SVEX-ALIST-EVAL-FIXPOINT-ITERATE-OF-APPEND-SVARLIST-X-ENV
 (78 6 (:REWRITE SV::SVEX-ENV-EXTRACT-WHEN-ALIST-KEYS-EQUAL))
 (24 24 (:TYPE-PRESCRIPTION SV::SVEX-ALIST-KEYS))
 (24 24 (:TYPE-PRESCRIPTION ALIST-KEYS))
 (24 6 (:REWRITE SV::SVEX-ENV-FIX-WHEN-SVEX-ENV-P))
 (24 4 (:DEFINITION BINARY-APPEND))
 (20 20 (:TYPE-PRESCRIPTION SV::SVARLIST-X-ENV))
 (20 8 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (18 6 (:REWRITE ZP-WHEN-GT-0))
 (12 12 (:TYPE-PRESCRIPTION SV::SVEX-ENV-P))
 (6 6 (:REWRITE ZP-WHEN-INTEGERP))
 (6 6 (:REWRITE ZP-OPEN))
 (6 6 (:REWRITE SV::SVEX-ENV-P-WHEN-NOT-CONSP))
 (6 6 (:REWRITE DEFAULT-<-2))
 (6 6 (:REWRITE DEFAULT-<-1))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 4 (:REWRITE DEFAULT-CAR))
 (4 4 (:REWRITE DEFAULT-+-2))
 (4 4 (:REWRITE DEFAULT-+-1))
 )
(SV::SVEX-ALIST-EVAL-LEAST-FIXPOINT-OF-APPEND-SVARLIST-X-ENV
 (14 14 (:TYPE-PRESCRIPTION BINARY-APPEND))
 (6 1 (:DEFINITION BINARY-APPEND))
 (5 2 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (2 2 (:REWRITE SV::SVEX-ALIST-EVAL-FIXPOINT-ITERATE-FIXPOINT-PRESERVED))
 (1 1 (:REWRITE DEFAULT-CDR))
 (1 1 (:REWRITE DEFAULT-CAR))
 )
(SV::SVEX-ENV-EXTRACT-APPEND-X-ENV
 (31 2 (:REWRITE SV::SVEX-ENV-FIX-OF-APPEND))
 (25 1 (:DEFINITION MEMBER-EQUAL))
 (24 4 (:REWRITE SV::SVARLIST-FIX-WHEN-SVARLIST-P))
 (20 1 (:REWRITE SV::SVEX-ENV-EXTRACT-KEYS-UNDER-SVEX-ENVS-EQUIVALENT))
 (19 1 (:REWRITE SV::SVEX-ENV-EXTRACT-WHEN-ALIST-KEYS-EQUAL))
 (18 3 (:DEFINITION BINARY-APPEND))
 (15 4 (:REWRITE SV::SVEX-ENV-FIX-WHEN-SVEX-ENV-P))
 (13 13 (:TYPE-PRESCRIPTION SV::SVARLIST-X-ENV))
 (12 12 (:TYPE-PRESCRIPTION SV::SVARLIST-FIX$INLINE))
 (10 2 (:REWRITE SV::SVAR-FIX-WHEN-SVAR-P))
 (9 6 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (8 8 (:TYPE-PRESCRIPTION SV::SVARLIST-P))
 (8 8 (:REWRITE SV::SVARLIST-P-WHEN-SUBSETP-EQUAL))
 (8 2 (:REWRITE MEMBER-WHEN-ATOM))
 (7 4 (:REWRITE DEFAULT-CDR))
 (7 4 (:REWRITE DEFAULT-CAR))
 (4 4 (:TYPE-PRESCRIPTION SV::SVEX-ENV-P))
 (4 4 (:TYPE-PRESCRIPTION SV::SVAR-P))
 (4 4 (:TYPE-PRESCRIPTION ALIST-KEYS))
 (4 4 (:REWRITE SV::SVARLIST-P-WHEN-NOT-CONSP))
 (4 4 (:REWRITE SV::SVAR-P-WHEN-MEMBER-EQUAL-OF-SVARLIST-P))
 (4 4 (:REWRITE SV::EXTRACT-SIMILAR-TO-NIL-IMPLIES-LOOKUP))
 (4 4 (:REWRITE SV::CONSP-OF-SVARLIST-FIX))
 (3 3 (:REWRITE SUBSETP-MEMBER . 4))
 (3 3 (:REWRITE SUBSETP-MEMBER . 3))
 (3 3 (:REWRITE SUBSETP-MEMBER . 2))
 (3 3 (:REWRITE INTERSECTP-MEMBER . 3))
 (3 3 (:REWRITE INTERSECTP-MEMBER . 2))
 (2 2 (:TYPE-PRESCRIPTION SV::SVAR-FIX$INLINE))
 (2 2 (:REWRITE SV::SVEX-ENV-P-OF-SVARLIST-X-ENV))
 (1 1 (:REWRITE-QUOTED-CONSTANT SV::SVEX-ENV-FIX-UNDER-SVEX-ENV-EQUIV))
 (1 1 (:REWRITE SV::SVEX-ENV-P-WHEN-NOT-CONSP))
 (1 1 (:REWRITE SUBSETP-TRANS2))
 (1 1 (:REWRITE SUBSETP-TRANS))
 (1 1 (:REWRITE SV::REWRITE-MEMBER-OF-APPEND-UNDER-SET-EQUIV))
 )
(SV::NETEVALCOMP-P-IMPLIES-<<=-FIXPOINT
 (42 2 (:DEFINITION MEMBER-EQUAL))
 (34 4 (:REWRITE SUBSETP-CAR-MEMBER))
 (18 3 (:DEFINITION BINARY-APPEND))
 (16 4 (:REWRITE NO-DUPLICATESP-EQUAL-NON-CONS))
 (12 12 (:TYPE-PRESCRIPTION SV::SVARLIST-X-ENV))
 (12 6 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (11 9 (:REWRITE DEFAULT-CDR))
 (10 4 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (10 4 (:REWRITE MEMBER-WHEN-ATOM))
 (9 9 (:REWRITE SV::SVEX-ALIST-KEYS-WHEN-EQUIV-COMPOSE))
 (9 7 (:REWRITE DEFAULT-CAR))
 (8 8 (:TYPE-PRESCRIPTION SUBSETP-EQUAL))
 (6 1 (:REWRITE SV::NETEVAL-ORDERING-FIX-WHEN-NETEVAL-ORDERING-P))
 (4 4 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (4 4 (:REWRITE SUBSETP-TRANS2))
 (4 4 (:REWRITE SUBSETP-TRANS))
 (4 4 (:REWRITE SUBSETP-MEMBER . 4))
 (4 4 (:REWRITE SUBSETP-MEMBER . 3))
 (4 4 (:REWRITE SUBSETP-MEMBER . 2))
 (4 4 (:REWRITE SUBSETP-MEMBER . 1))
 (4 4 (:REWRITE INTERSECTP-MEMBER . 3))
 (4 4 (:REWRITE INTERSECTP-MEMBER . 2))
 (4 4 (:REWRITE FN-CHECK-DEF-FORMALS))
 (3 3 (:REWRITE SV::SVEX-ALIST-WIDTH-WHEN-WIDTH-LIMITED-P))
 (2 2 (:TYPE-PRESCRIPTION SV::NETEVAL-ORDERING-P))
 (2 2 (:REWRITE-QUOTED-CONSTANT SV::SVEX-ENV-FIX-UNDER-SVEX-ENV-EQUIV))
 (2 2 (:REWRITE SV::REWRITE-MEMBER-OF-APPEND-UNDER-SET-EQUIV))
 (2 2 (:REWRITE SV::NETEVAL-ORDERING-P-WHEN-SUBSETP-EQUAL))
 (1 1 (:REWRITE SV::NETEVAL-ORDERING-P-WHEN-NOT-CONSP))
 )
