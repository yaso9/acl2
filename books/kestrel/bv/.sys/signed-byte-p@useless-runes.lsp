(SIGNED-BYTE-P-CASES
 (4 4 (:REWRITE DEFAULT-<-2))
 (4 4 (:REWRITE DEFAULT-<-1))
 )
(SIGNED-BYTE-WHEN-NOT-INTEGERP-CHEAP)
(SIGNED-BYTE-WHEN-<=-OF-0-CHEAP)
(SIGNED-BYTE-P-OF-+-OF-1
 (402 402 (:TYPE-PRESCRIPTION EVENP))
 (268 134 (:TYPE-PRESCRIPTION EXPT-TYPE-ODD-EXPONENT-NEGATIVE-BASE))
 (268 134 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-2))
 (268 134 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-1))
 (134 134 (:TYPE-PRESCRIPTION EXPT-TYPE-SMALL-BASE-NEGATIVE-EXPONENT))
 (76 9 (:REWRITE DEFAULT-<-2))
 (36 3 (:REWRITE DEFAULT-UNARY-MINUS))
 (9 9 (:REWRITE DEFAULT-<-1))
 (4 4 (:REWRITE DEFAULT-+-2))
 (4 4 (:REWRITE DEFAULT-+-1))
 (3 3 (:REWRITE EXPT-WHEN-NOT-INTEGERP-ARG1-CHEAP))
 (3 3 (:REWRITE EXPT-WHEN-NOT-ACL2-NUMBERP-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-NOT-INTEGERP-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-<=-OF-0-CHEAP))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(SIGNED-BYTE-P-WHEN-SIGNED-BYTE-P
 (549 549 (:TYPE-PRESCRIPTION EVENP))
 (366 183 (:TYPE-PRESCRIPTION EXPT-TYPE-ODD-EXPONENT-NEGATIVE-BASE))
 (366 183 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-2))
 (366 183 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-1))
 (183 183 (:TYPE-PRESCRIPTION EXPT-TYPE-SMALL-BASE-NEGATIVE-EXPONENT))
 (144 4 (:LINEAR EXPT-HALF-LINEAR))
 (72 28 (:REWRITE DEFAULT-<-2))
 (48 4 (:REWRITE DEFAULT-*-2))
 (42 42 (:REWRITE EXPT-WHEN-NOT-INTEGERP-ARG1-CHEAP))
 (42 42 (:REWRITE EXPT-WHEN-NOT-ACL2-NUMBERP-CHEAP))
 (28 28 (:REWRITE DEFAULT-<-1))
 (24 8 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR))
 (24 2 (:REWRITE DEFAULT-UNARY-MINUS))
 (16 16 (:REWRITE DEFAULT-+-2))
 (16 16 (:REWRITE DEFAULT-+-1))
 (12 4 (:REWRITE +-OF-EXPT2-OF-ONE-LESS-AND-EXPT2-OF-ONE-LESS))
 (12 4 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-4))
 (12 4 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-2))
 (8 8 (:LINEAR <=-OF-EXPT-AND-EXPT-SAME-BASE-LINEAR))
 (8 8 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR-NEGATIVE-EXPONENT))
 (8 8 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-BASE-LINEAR))
 (4 4 (:REWRITE DEFAULT-*-1))
 (4 4 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-3))
 (4 4 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-1))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-NOT-INTEGERP-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-<=-OF-0-CHEAP))
 )
(SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P-ONE-LESS
 (228 228 (:TYPE-PRESCRIPTION EVENP))
 (152 76 (:TYPE-PRESCRIPTION EXPT-TYPE-ODD-EXPONENT-NEGATIVE-BASE))
 (152 76 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-2))
 (152 76 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-1))
 (76 76 (:TYPE-PRESCRIPTION EXPT-TYPE-SMALL-BASE-NEGATIVE-EXPONENT))
 (45 12 (:REWRITE DEFAULT-<-2))
 (36 1 (:LINEAR EXPT-HALF-LINEAR))
 (16 16 (:REWRITE EXPT-WHEN-NOT-INTEGERP-ARG1-CHEAP))
 (16 16 (:REWRITE EXPT-WHEN-NOT-ACL2-NUMBERP-CHEAP))
 (12 12 (:REWRITE DEFAULT-<-1))
 (12 1 (:REWRITE DEFAULT-UNARY-MINUS))
 (12 1 (:REWRITE DEFAULT-*-2))
 (8 8 (:REWRITE DEFAULT-+-2))
 (8 8 (:REWRITE DEFAULT-+-1))
 (8 4 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR))
 (4 4 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR-NEGATIVE-EXPONENT))
 (4 2 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-4))
 (4 2 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-2))
 (3 1 (:REWRITE +-OF-EXPT2-OF-ONE-LESS-AND-EXPT2-OF-ONE-LESS))
 (2 2 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-3))
 (2 2 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-1))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-NOT-INTEGERP-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-<=-OF-0-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-SIGNED-BYTE-P))
 (1 1 (:REWRITE DEFAULT-*-1))
 )
(SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P-SMALLER-FREE
 (357 357 (:TYPE-PRESCRIPTION EVENP))
 (238 119 (:TYPE-PRESCRIPTION EXPT-TYPE-ODD-EXPONENT-NEGATIVE-BASE))
 (238 119 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-2))
 (238 119 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-1))
 (119 119 (:TYPE-PRESCRIPTION EXPT-TYPE-SMALL-BASE-NEGATIVE-EXPONENT))
 (51 18 (:REWRITE DEFAULT-<-2))
 (36 1 (:LINEAR EXPT-HALF-LINEAR))
 (29 29 (:REWRITE EXPT-WHEN-NOT-INTEGERP-ARG1-CHEAP))
 (29 29 (:REWRITE EXPT-WHEN-NOT-ACL2-NUMBERP-CHEAP))
 (18 18 (:REWRITE DEFAULT-<-1))
 (14 6 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR))
 (12 1 (:REWRITE DEFAULT-UNARY-MINUS))
 (12 1 (:REWRITE DEFAULT-*-2))
 (7 7 (:REWRITE DEFAULT-+-2))
 (7 7 (:REWRITE DEFAULT-+-1))
 (7 3 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-4))
 (7 3 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-2))
 (6 6 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR-NEGATIVE-EXPONENT))
 (3 3 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-3))
 (3 3 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-1))
 (3 1 (:REWRITE +-OF-EXPT2-OF-ONE-LESS-AND-EXPT2-OF-ONE-LESS))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-NOT-INTEGERP-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-<=-OF-0-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-SIGNED-BYTE-P))
 (1 1 (:REWRITE DEFAULT-*-1))
 )
(SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P
 (189 189 (:TYPE-PRESCRIPTION EVENP))
 (126 63 (:TYPE-PRESCRIPTION EXPT-TYPE-ODD-EXPONENT-NEGATIVE-BASE))
 (126 63 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-2))
 (126 63 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-1))
 (63 63 (:TYPE-PRESCRIPTION EXPT-TYPE-SMALL-BASE-NEGATIVE-EXPONENT))
 (54 10 (:REWRITE DEFAULT-<-2))
 (12 1 (:REWRITE DEFAULT-UNARY-MINUS))
 (10 10 (:REWRITE DEFAULT-<-1))
 (9 9 (:REWRITE EXPT-WHEN-NOT-INTEGERP-ARG1-CHEAP))
 (9 9 (:REWRITE EXPT-WHEN-NOT-ACL2-NUMBERP-CHEAP))
 (2 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:REWRITE DEFAULT-+-1))
 (2 2 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR-NEGATIVE-EXPONENT))
 (2 2 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-NOT-INTEGERP-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-<=-OF-0-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P-SMALLER-FREE))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-SIGNED-BYTE-P))
 (1 1 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-4))
 (1 1 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-3))
 (1 1 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-2))
 (1 1 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-1))
 (1 1 (:LINEAR EXPT-BOUND-LINEAR-2))
 )
(SIGNED-BYTE-P-LONGER
 (621 621 (:TYPE-PRESCRIPTION EVENP))
 (414 207 (:TYPE-PRESCRIPTION EXPT-TYPE-ODD-EXPONENT-NEGATIVE-BASE))
 (414 207 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-2))
 (414 207 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-1))
 (340 1 (:REWRITE SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P))
 (338 1 (:DEFINITION UNSIGNED-BYTE-P))
 (207 207 (:TYPE-PRESCRIPTION EXPT-TYPE-SMALL-BASE-NEGATIVE-EXPONENT))
 (144 4 (:LINEAR EXPT-HALF-LINEAR))
 (85 30 (:REWRITE DEFAULT-<-2))
 (64 12 (:LINEAR <=-OF-EXPT-AND-EXPT-SAME-BASE-LINEAR))
 (57 57 (:REWRITE EXPT-WHEN-NOT-INTEGERP-ARG1-CHEAP))
 (57 57 (:REWRITE EXPT-WHEN-NOT-ACL2-NUMBERP-CHEAP))
 (48 4 (:REWRITE DEFAULT-*-2))
 (30 30 (:REWRITE DEFAULT-<-1))
 (28 12 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR))
 (25 12 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-BASE-LINEAR))
 (24 2 (:REWRITE DEFAULT-UNARY-MINUS))
 (16 16 (:REWRITE DEFAULT-+-2))
 (16 16 (:REWRITE DEFAULT-+-1))
 (14 6 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-4))
 (14 6 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-2))
 (12 12 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR-NEGATIVE-EXPONENT))
 (12 4 (:REWRITE +-OF-EXPT2-OF-ONE-LESS-AND-EXPT2-OF-ONE-LESS))
 (6 6 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-3))
 (6 6 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-1))
 (4 4 (:REWRITE DEFAULT-*-1))
 (1 1 (:TYPE-PRESCRIPTION UNSIGNED-BYTE-P))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-NOT-INTEGERP-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-<=-OF-0-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P-SMALLER-FREE))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-SIGNED-BYTE-P))
 )
(SBP-32-WHEN-NON-NEG
 (7 6 (:REWRITE DEFAULT-<-1))
 (6 6 (:REWRITE DEFAULT-<-2))
 (6 1 (:REWRITE SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-NOT-INTEGERP-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-<=-OF-0-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P-SMALLER-FREE))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-SIGNED-BYTE-P))
 (1 1 (:REWRITE SIGNED-BYTE-P-LONGER))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(SBP-UBP-HACK
 (6 6 (:REWRITE DEFAULT-<-2))
 (6 6 (:REWRITE DEFAULT-<-1))
 (6 1 (:REWRITE SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P))
 (3 1 (:REWRITE SBP-32-WHEN-NON-NEG))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-NOT-INTEGERP-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-<=-OF-0-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P-SMALLER-FREE))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-SIGNED-BYTE-P))
 (1 1 (:REWRITE SIGNED-BYTE-P-LONGER))
 (1 1 (:REWRITE DEFAULT-+-2))
 (1 1 (:REWRITE DEFAULT-+-1))
 )
(SIGNED-BYTE-P-OF-PLUS-CONSTANT
 (9 8 (:REWRITE DEFAULT-<-1))
 (8 8 (:REWRITE DEFAULT-<-2))
 (6 1 (:REWRITE SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P))
 (4 1 (:DEFINITION UNSIGNED-BYTE-P))
 (3 1 (:REWRITE SBP-32-WHEN-NON-NEG))
 (2 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:REWRITE DEFAULT-+-1))
 (1 1 (:TYPE-PRESCRIPTION UNSIGNED-BYTE-P))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-NOT-INTEGERP-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-<=-OF-0-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P-SMALLER-FREE))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-SIGNED-BYTE-P))
 (1 1 (:REWRITE SIGNED-BYTE-P-LONGER))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (1 1 (:REWRITE DEFAULT-UNARY-MINUS))
 )
(SIGNED-BYTE-P-FORWARD-ARG1
 (3 3 (:TYPE-PRESCRIPTION EVENP))
 (2 1 (:TYPE-PRESCRIPTION EXPT-TYPE-ODD-EXPONENT-NEGATIVE-BASE))
 (2 1 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-2))
 (2 1 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-1))
 (1 1 (:TYPE-PRESCRIPTION EXPT-TYPE-SMALL-BASE-NEGATIVE-EXPONENT))
 )
(SIGNED-BYTE-P-OF-+
 (2400 2400 (:TYPE-PRESCRIPTION EVENP))
 (1600 800 (:TYPE-PRESCRIPTION EXPT-TYPE-ODD-EXPONENT-NEGATIVE-BASE))
 (1600 800 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-2))
 (1600 800 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-1))
 (800 800 (:TYPE-PRESCRIPTION EXPT-TYPE-SMALL-BASE-NEGATIVE-EXPONENT))
 (266 52 (:REWRITE DEFAULT-<-2))
 (168 14 (:REWRITE DEFAULT-*-2))
 (140 1 (:REWRITE SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P))
 (138 1 (:DEFINITION UNSIGNED-BYTE-P))
 (110 9 (:REWRITE DEFAULT-UNARY-MINUS))
 (52 52 (:REWRITE DEFAULT-<-1))
 (38 38 (:REWRITE EXPT-WHEN-NOT-INTEGERP-ARG1-CHEAP))
 (38 38 (:REWRITE EXPT-WHEN-NOT-ACL2-NUMBERP-CHEAP))
 (29 25 (:REWRITE DEFAULT-+-2))
 (25 25 (:REWRITE DEFAULT-+-1))
 (22 10 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR-NEGATIVE-EXPONENT))
 (18 10 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR))
 (14 14 (:REWRITE DEFAULT-*-1))
 (11 5 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-4))
 (11 5 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-3))
 (11 5 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-2))
 (11 5 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-1))
 (10 10 (:LINEAR EXPT-BOUND-LINEAR-WEAK))
 (10 10 (:LINEAR <=-OF-EXPT-AND-EXPT-SAME-BASE-LINEAR))
 (7 7 (:REWRITE <-OF-CONSTANT-AND-*-OF-CONSTANT))
 (5 5 (:LINEAR EXPT-BOUND-LINEAR-2))
 (4 4 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (1 1 (:TYPE-PRESCRIPTION UNSIGNED-BYTE-P))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-NOT-INTEGERP-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-<=-OF-0-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P-SMALLER-FREE))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-SIGNED-BYTE-P))
 (1 1 (:REWRITE SIGNED-BYTE-P-LONGER))
 )
(SIGNED-BYTE-P-IN-TERMS-OF-FLOOR
 (1380 1380 (:TYPE-PRESCRIPTION EVENP))
 (920 460 (:TYPE-PRESCRIPTION EXPT-TYPE-ODD-EXPONENT-NEGATIVE-BASE))
 (920 460 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-2))
 (920 460 (:TYPE-PRESCRIPTION EXPT-TYPE-EVEN-EXPONENT-1))
 (536 2 (:REWRITE FLOOR-UNIQUE-EQUAL-VERSION))
 (460 460 (:TYPE-PRESCRIPTION EXPT-TYPE-SMALL-BASE-NEGATIVE-EXPONENT))
 (414 5 (:REWRITE FLOOR-WHEN-<))
 (228 63 (:REWRITE DEFAULT-<-2))
 (180 5 (:LINEAR EXPT-HALF-LINEAR))
 (174 3 (:LINEAR <-OF-*-SAME-LINEAR-SPECIAL))
 (174 3 (:LINEAR <-OF-*-SAME-LINEAR-2))
 (156 13 (:REWRITE DEFAULT-*-2))
 (135 1 (:REWRITE SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P))
 (133 1 (:DEFINITION UNSIGNED-BYTE-P))
 (118 63 (:REWRITE DEFAULT-<-1))
 (92 4 (:REWRITE DEFAULT-UNARY-/))
 (86 86 (:TYPE-PRESCRIPTION <=-OF-0-AND-FLOOR-WHEN-BOTH-NONPOSITIVE-TYPE))
 (86 86 (:TYPE-PRESCRIPTION <-OF-FLOOR-AND-0-WHEN-POSITIVE-AND-NEGATIVE-TYPE))
 (74 74 (:REWRITE EXPT-WHEN-NOT-INTEGERP-ARG1-CHEAP))
 (74 74 (:REWRITE EXPT-WHEN-NOT-ACL2-NUMBERP-CHEAP))
 (72 6 (:REWRITE FLOOR-WHEN-NOT-RATIONALP-OF-QUOTIENT))
 (48 4 (:REWRITE DEFAULT-UNARY-MINUS))
 (48 1 (:LINEAR FLOOR-BOUND-STRICT-LINEAR))
 (46 22 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR))
 (42 31 (:REWRITE DEFAULT-+-2))
 (35 22 (:LINEAR EXPT-BOUND-LINEAR-WEAK))
 (35 22 (:LINEAR <=-OF-EXPT-AND-EXPT-SAME-BASE-LINEAR))
 (35 13 (:REWRITE DEFAULT-*-1))
 (34 34 (:TYPE-PRESCRIPTION <=-OF-FLOOR-WHEN-<-TYPE))
 (31 31 (:REWRITE DEFAULT-+-1))
 (23 11 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-4))
 (23 11 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-2))
 (22 22 (:LINEAR <-OF-EXPT-AND-EXPT-SAME-EXPONENTS-LINEAR-NEGATIVE-EXPONENT))
 (15 5 (:REWRITE +-OF-EXPT2-OF-ONE-LESS-AND-EXPT2-OF-ONE-LESS))
 (11 11 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-3))
 (11 11 (:LINEAR EXPT-WHEN-NEGATIVE-EXPONENT-LINEAR-1))
 (6 6 (:LINEAR <=-OF-*-AND-*-SAME-LINEAR))
 (6 6 (:LINEAR <=-OF-*-AND-*-SAME-ALT-LINEAR))
 (6 6 (:LINEAR <-OF-*-AND-*-LINEAR))
 (5 5 (:REWRITE FLOOR-WHEN-NOT-RATIONALP-ARG1))
 (5 5 (:REWRITE FLOOR-WHEN-I-IS-NOT-AN-ACL2-NUMBERP))
 (5 5 (:REWRITE FLOOR-MINUS-NEGATIVE-CONSTANT))
 (3 3 (:LINEAR <-OF-*-AND-*-SAME-LINEAR-4))
 (3 3 (:LINEAR <-OF-*-AND-*-SAME-LINEAR-3))
 (3 3 (:LINEAR <-OF-*-AND-*-SAME-LINEAR-2))
 (3 1 (:REWRITE <-OF-1-AND-EXPT-GEN))
 (2 2 (:REWRITE <-OF-*-OF-CONSTANT-AND-CONSTANT))
 (1 1 (:TYPE-PRESCRIPTION UNSIGNED-BYTE-P))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-NOT-INTEGERP-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-WHEN-<=-OF-0-CHEAP))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-UNSIGNED-BYTE-P-SMALLER-FREE))
 (1 1 (:REWRITE SIGNED-BYTE-P-WHEN-SIGNED-BYTE-P))
 (1 1 (:REWRITE SIGNED-BYTE-P-LONGER))
 (1 1 (:LINEAR FLOOR-WEAK-MONOTONE-LINEAR=-2))
 (1 1 (:LINEAR FLOOR-WEAK-MONOTONE-LINEAR-1))
 (1 1 (:LINEAR FLOOR-BOUND-ARG1-LINEAR))
 )
