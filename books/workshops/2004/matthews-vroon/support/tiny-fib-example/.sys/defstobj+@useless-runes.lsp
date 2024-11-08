(FIRSTN)
(LASTN
 (5 5 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (5 5 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (4 4 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (4 4 (:REWRITE |(< (- x) (- y))|))
 (2 2 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (2 2 (:REWRITE DEFAULT-UNARY-MINUS))
 (2 2 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE DEFAULT-<-1))
 (2 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:REWRITE DEFAULT-+-1))
 (1 1 (:REWRITE ZP-OPEN))
 (1 1 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (1 1 (:REWRITE |(< d (+ c x))|))
 (1 1 (:REWRITE |(< (- x) 0)|))
 )
(CAR-LASTN
 (6 6 (:REWRITE ZP-OPEN))
 (4 4 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (4 4 (:REWRITE NORMALIZE-ADDENDS))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 4 (:REWRITE DEFAULT-+-2))
 (4 4 (:REWRITE DEFAULT-+-1))
 (3 3 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (3 3 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (3 3 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (3 3 (:REWRITE |(equal (- x) (- y))|))
 )
(CDR-LASTN
 (52 52 (:REWRITE DEFAULT-CDR))
 (19 19 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (19 19 (:REWRITE NORMALIZE-ADDENDS))
 (19 19 (:REWRITE DEFAULT-+-2))
 (19 19 (:REWRITE DEFAULT-+-1))
 (14 14 (:REWRITE ZP-OPEN))
 (10 10 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (10 10 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (10 10 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (10 10 (:REWRITE |(equal (- x) (- y))|))
 (9 9 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (9 9 (:REWRITE SIMPLIFY-SUMS-<))
 (9 9 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (9 9 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (9 9 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (9 9 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (9 9 (:REWRITE DEFAULT-<-2))
 (9 9 (:REWRITE DEFAULT-<-1))
 (9 9 (:REWRITE |(< (- x) 0)|))
 (9 9 (:REWRITE |(< (- x) (- y))|))
 (8 8 (:REWRITE REDUCE-INTEGERP-+))
 (8 8 (:REWRITE INTEGERP-MINUS-X))
 (8 8 (:META META-INTEGERP-CORRECT))
 )
(CONSP-LASTN
 (18 14 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (17 10 (:REWRITE DEFAULT-+-2))
 (16 10 (:REWRITE DEFAULT-<-2))
 (15 11 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (13 10 (:REWRITE SIMPLIFY-SUMS-<))
 (11 11 (:REWRITE |(< (- x) (- y))|))
 (10 10 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (10 10 (:REWRITE NORMALIZE-ADDENDS))
 (10 10 (:REWRITE DEFAULT-<-1))
 (10 10 (:REWRITE DEFAULT-+-1))
 (8 4 (:REWRITE |(< d (+ c x))|))
 (5 5 (:REWRITE ZP-OPEN))
 (4 4 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 4 (:REWRITE |(< (- x) 0)|))
 (1 1 (:REWRITE |(< (+ c x) d)|))
 )
(LASTN-ALT-DEF
 (48 6 (:DEFINITION NTH))
 (28 21 (:REWRITE DEFAULT-CDR))
 (28 4 (:DEFINITION LEN))
 (19 15 (:REWRITE DEFAULT-+-2))
 (16 8 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (15 15 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (15 15 (:REWRITE NORMALIZE-ADDENDS))
 (15 15 (:REWRITE DEFAULT-+-1))
 (14 10 (:REWRITE DEFAULT-CAR))
 (12 8 (:REWRITE SIMPLIFY-SUMS-<))
 (12 8 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (12 8 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (12 8 (:REWRITE DEFAULT-<-2))
 (10 10 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (10 10 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (10 10 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (10 10 (:REWRITE |(equal (- x) (- y))|))
 (8 8 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (8 8 (:REWRITE DEFAULT-<-1))
 (8 8 (:REWRITE |(< (- x) (- y))|))
 (8 4 (:DEFINITION TRUE-LISTP))
 (6 6 (:REWRITE ZP-OPEN))
 (4 4 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (4 4 (:REWRITE REDUCE-INTEGERP-+))
 (4 4 (:REWRITE INTEGERP-MINUS-X))
 (4 4 (:REWRITE |(< (- x) 0)|))
 (4 4 (:META META-INTEGERP-CORRECT))
 )
(LASTN-NIL
 (40 5 (:REWRITE LASTN-ALT-DEF))
 (22 13 (:REWRITE DEFAULT-+-2))
 (20 12 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (18 11 (:REWRITE SIMPLIFY-SUMS-<))
 (18 11 (:REWRITE DEFAULT-<-2))
 (15 13 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (13 13 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (13 13 (:REWRITE NORMALIZE-ADDENDS))
 (13 13 (:REWRITE DEFAULT-+-1))
 (12 12 (:REWRITE |(< (- x) (- y))|))
 (11 11 (:REWRITE DEFAULT-<-1))
 (10 5 (:REWRITE |(< d (+ c x))|))
 (9 9 (:REWRITE DEFAULT-CDR))
 (6 6 (:REWRITE ZP-OPEN))
 (5 5 (:REWRITE REDUCE-INTEGERP-+))
 (5 5 (:REWRITE INTEGERP-MINUS-X))
 (5 5 (:META META-INTEGERP-CORRECT))
 (3 3 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (3 3 (:REWRITE |(< (- x) 0)|))
 (1 1 (:REWRITE |(< (+ c x) d)|))
 )
(LASTN-0
 (10 10 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (10 2 (:DEFINITION TRUE-LISTP))
 (8 1 (:REWRITE LASTN-NIL))
 (8 1 (:REWRITE LASTN-ALT-DEF))
 (2 2 (:REWRITE DEFAULT-CDR))
 )
(NTH-UPDATE-NTH2
 (16 2 (:DEFINITION NTH))
 (10 10 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (10 10 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (10 10 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (10 10 (:REWRITE |(equal (- x) (- y))|))
 (9 1 (:DEFINITION UPDATE-NTH))
 (8 8 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-=-0))
 (8 8 (:REWRITE |(equal (- x) 0)|))
 (4 4 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (4 4 (:REWRITE SIMPLIFY-SUMS-<))
 (4 4 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (4 4 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (4 4 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (4 4 (:REWRITE REDUCE-INTEGERP-+))
 (4 4 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (4 4 (:REWRITE INTEGERP-MINUS-X))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 4 (:REWRITE DEFAULT-<-2))
 (4 4 (:REWRITE DEFAULT-<-1))
 (4 4 (:REWRITE |(< (- x) 0)|))
 (4 4 (:REWRITE |(< (- x) (- y))|))
 (4 4 (:META META-INTEGERP-CORRECT))
 (3 3 (:REWRITE ZP-OPEN))
 (3 3 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (3 3 (:REWRITE NORMALIZE-ADDENDS))
 (3 3 (:REWRITE DEFAULT-CAR))
 (3 3 (:REWRITE DEFAULT-+-2))
 (3 3 (:REWRITE DEFAULT-+-1))
 )
(NTH-UPDATE-NTH-1
 (134 134 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (134 134 (:REWRITE DEFAULT-+-2))
 (134 134 (:REWRITE DEFAULT-+-1))
 (131 131 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (131 131 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (81 81 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (81 81 (:REWRITE |(< (- x) (- y))|))
 (80 80 (:REWRITE SIMPLIFY-SUMS-<))
 (80 80 (:REWRITE DEFAULT-<-2))
 (80 80 (:REWRITE DEFAULT-<-1))
 (44 44 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (44 44 (:REWRITE |(equal (- x) (- y))|))
 (31 25 (:REWRITE REDUCE-INTEGERP-+))
 (26 26 (:META META-INTEGERP-CORRECT))
 (25 25 (:REWRITE INTEGERP-MINUS-X))
 (22 22 (:REWRITE |(< (- x) 0)|))
 (21 21 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (9 9 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-=-0))
 (9 9 (:REWRITE |(equal (- x) 0)|))
 (6 6 (:REWRITE |(equal (+ c x) (+ d y))|))
 (4 4 (:REWRITE DEFAULT-UNARY-MINUS))
 )
(NTH-UPDATE-NTH-2
 (10 10 (:REWRITE DEFAULT-CDR))
 (7 7 (:REWRITE DEFAULT-CAR))
 (5 5 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (5 5 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (5 5 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (5 5 (:REWRITE |(equal (- x) (- y))|))
 (4 4 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-=-0))
 (4 4 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (4 4 (:REWRITE SIMPLIFY-SUMS-<))
 (4 4 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (4 4 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (4 4 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (4 4 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (4 4 (:REWRITE DEFAULT-<-2))
 (4 4 (:REWRITE DEFAULT-<-1))
 (4 4 (:REWRITE |(equal (- x) 0)|))
 (4 4 (:REWRITE |(< (- x) 0)|))
 (4 4 (:REWRITE |(< (- x) (- y))|))
 (2 2 (:REWRITE REDUCE-INTEGERP-+))
 (2 2 (:REWRITE INTEGERP-MINUS-X))
 (2 2 (:META META-INTEGERP-CORRECT))
 (1 1 (:REWRITE ZP-OPEN))
 (1 1 (:REWRITE NTH-UPDATE-NTH-1))
 (1 1 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (1 1 (:REWRITE NORMALIZE-ADDENDS))
 (1 1 (:REWRITE DEFAULT-+-2))
 (1 1 (:REWRITE DEFAULT-+-1))
 )
(REPEAT)
(UPDATE-NTH-NIL
 (26 10 (:REWRITE DEFAULT-CDR))
 (26 10 (:REWRITE DEFAULT-CAR))
 (9 3 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (9 3 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (9 3 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (6 6 (:REWRITE ZP-OPEN))
 (4 4 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (4 4 (:REWRITE NORMALIZE-ADDENDS))
 (4 4 (:REWRITE DEFAULT-+-2))
 (4 4 (:REWRITE DEFAULT-+-1))
 (3 3 (:REWRITE |(equal (- x) (- y))|))
 )
(UPDATE-NTH-NTH-NOOP-HELPER
 (19 12 (:REWRITE DEFAULT-+-2))
 (18 14 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (16 10 (:REWRITE DEFAULT-<-2))
 (15 11 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (13 10 (:REWRITE SIMPLIFY-SUMS-<))
 (12 12 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (12 12 (:REWRITE NORMALIZE-ADDENDS))
 (12 12 (:REWRITE DEFAULT-+-1))
 (12 3 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (12 3 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (12 3 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (11 11 (:REWRITE |(< (- x) (- y))|))
 (10 10 (:REWRITE DEFAULT-CDR))
 (10 10 (:REWRITE DEFAULT-<-1))
 (8 4 (:REWRITE |(< d (+ c x))|))
 (7 7 (:REWRITE ZP-OPEN))
 (6 6 (:REWRITE DEFAULT-CAR))
 (4 4 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (4 4 (:REWRITE |(< (- x) 0)|))
 (3 3 (:REWRITE |(equal (- x) (- y))|))
 (1 1 (:REWRITE |(< (+ c x) d)|))
 )
(UPDATE-NTH-NTH-NOOP
 (46 23 (:TYPE-PRESCRIPTION TRUE-LISTP-UPDATE-NTH))
 (23 23 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (7 1 (:DEFINITION LEN))
 (5 4 (:REWRITE DEFAULT-+-2))
 (4 4 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (4 4 (:REWRITE NORMALIZE-ADDENDS))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 4 (:REWRITE DEFAULT-+-1))
 (4 2 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (3 3 (:REWRITE ZP-OPEN))
 (3 3 (:REWRITE DEFAULT-CAR))
 (3 2 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE SIMPLIFY-SUMS-<))
 (2 2 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (2 2 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (2 2 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (2 2 (:REWRITE DEFAULT-<-1))
 (2 2 (:REWRITE |(< (- x) (- y))|))
 (1 1 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (1 1 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (1 1 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (1 1 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (1 1 (:REWRITE |(equal (- x) (- y))|))
 (1 1 (:REWRITE |(< (- x) 0)|))
 )
(UPDATE-NTH-UPDATE-NTH-DIFF
 (279 39 (:REWRITE ZP-OPEN))
 (180 20 (:REWRITE |(< d (+ c x))|))
 (144 45 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (143 44 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (85 85 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (85 85 (:REWRITE NORMALIZE-ADDENDS))
 (85 85 (:REWRITE DEFAULT-+-2))
 (85 85 (:REWRITE DEFAULT-+-1))
 (63 63 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (63 63 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (45 45 (:REWRITE |(equal (- x) (- y))|))
 (42 42 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (42 42 (:REWRITE |(< (- x) (- y))|))
 (41 41 (:REWRITE SIMPLIFY-SUMS-<))
 (41 41 (:REWRITE DEFAULT-<-2))
 (41 41 (:REWRITE DEFAULT-<-1))
 (38 2 (:DEFINITION REPEAT))
 (27 23 (:REWRITE REDUCE-INTEGERP-+))
 (24 24 (:META META-INTEGERP-CORRECT))
 (23 23 (:REWRITE INTEGERP-MINUS-X))
 (17 17 (:REWRITE |(< (- x) 0)|))
 (16 16 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (11 11 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-=-0))
 (11 11 (:REWRITE |(equal (- x) 0)|))
 (10 2 (:DEFINITION BINARY-APPEND))
 (5 5 (:TYPE-PRESCRIPTION REPEAT))
 (2 1 (:TYPE-PRESCRIPTION TRUE-LISTP-APPEND))
 (1 1 (:TYPE-PRESCRIPTION BINARY-APPEND))
 (1 1 (:REWRITE |(equal (+ d x) (+ c y))|))
 (1 1 (:REWRITE |(equal (+ c x) (+ d y))|))
 )
(UPDATE-NTH-UPDATE-NTH-SAME
 (73 22 (:REWRITE DEFAULT-CDR))
 (60 15 (:REWRITE DEFAULT-CAR))
 (30 3 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (30 3 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (30 3 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (8 8 (:REWRITE ZP-OPEN))
 (7 7 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (7 7 (:REWRITE NORMALIZE-ADDENDS))
 (7 7 (:REWRITE DEFAULT-+-2))
 (7 7 (:REWRITE DEFAULT-+-1))
 (3 3 (:REWRITE |(equal (- x) (- y))|))
 )
(LEN-REPEAT
 (16 10 (:REWRITE DEFAULT-+-2))
 (14 7 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (12 6 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (10 10 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (10 10 (:REWRITE NORMALIZE-ADDENDS))
 (10 10 (:REWRITE DEFAULT-+-1))
 (7 7 (:REWRITE |(equal (- x) (- y))|))
 (6 5 (:REWRITE DEFAULT-CDR))
 (5 5 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (4 4 (:REWRITE ZP-OPEN))
 (4 4 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (4 4 (:REWRITE SIMPLIFY-SUMS-<))
 (4 4 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (4 4 (:REWRITE REDUCE-INTEGERP-+))
 (4 4 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (4 4 (:REWRITE INTEGERP-MINUS-X))
 (4 4 (:REWRITE DEFAULT-<-2))
 (4 4 (:REWRITE DEFAULT-<-1))
 (4 4 (:REWRITE |(< (- x) 0)|))
 (4 4 (:REWRITE |(< (- x) (- y))|))
 (4 4 (:META META-INTEGERP-CORRECT))
 (3 2 (:REWRITE |(equal (+ c x) d)|))
 (2 2 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-=-0))
 (2 2 (:REWRITE |(equal (- x) 0)|))
 )
(LEN-UPDATE-NTH-BETTER)
(CAR-UPDATE-NTH
 (19 1 (:DEFINITION REPEAT))
 (8 8 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (8 8 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (8 8 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (8 8 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (8 8 (:REWRITE NORMALIZE-ADDENDS))
 (8 8 (:REWRITE DEFAULT-+-2))
 (8 8 (:REWRITE DEFAULT-+-1))
 (8 8 (:REWRITE |(equal (- x) (- y))|))
 (5 1 (:REWRITE |(+ c (+ d x))|))
 (5 1 (:DEFINITION BINARY-APPEND))
 (4 4 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (4 4 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (3 3 (:TYPE-PRESCRIPTION REPEAT))
 (2 2 (:REWRITE SIMPLIFY-SUMS-<))
 (2 2 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (2 2 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (2 2 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE DEFAULT-<-1))
 (2 2 (:REWRITE CDR-CONS))
 (2 2 (:REWRITE |(< (- x) (- y))|))
 (2 1 (:TYPE-PRESCRIPTION TRUE-LISTP-APPEND))
 (1 1 (:TYPE-PRESCRIPTION BINARY-APPEND))
 )
(CDR-UPDATE-NTH
 (29 5 (:REWRITE ZP-OPEN))
 (18 2 (:REWRITE |(< d (+ c x))|))
 (14 11 (:REWRITE DEFAULT-CDR))
 (10 2 (:REWRITE |(+ c (+ d x))|))
 (5 5 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (5 5 (:REWRITE NORMALIZE-ADDENDS))
 (5 5 (:REWRITE DEFAULT-+-2))
 (5 5 (:REWRITE DEFAULT-+-1))
 (5 2 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (5 2 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (5 2 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (4 4 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (4 4 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (4 4 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE SIMPLIFY-SUMS-<))
 (2 2 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (2 2 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (2 2 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE DEFAULT-<-1))
 (2 2 (:REWRITE |(equal (- x) (- y))|))
 (2 2 (:REWRITE |(< (- x) (- y))|))
 (2 2 (:DEFINITION NOT))
 )
(COPY-FROM-STOBJ-ARRAY)
(COPY-FROM-STOBJ-ARRAY-NOOP-L1
 (698 51 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (315 35 (:REWRITE SIMPLIFY-SUMS-<))
 (220 5 (:DEFINITION NTH))
 (201 100 (:REWRITE NORMALIZE-ADDENDS))
 (196 28 (:DEFINITION LEN))
 (167 9 (:REWRITE ZP-OPEN))
 (137 19 (:REWRITE |(+ y (+ x z))|))
 (102 62 (:REWRITE DEFAULT-+-2))
 (86 51 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (62 62 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (62 62 (:REWRITE DEFAULT-+-1))
 (57 16 (:REWRITE BUBBLE-DOWN-+-MATCH-3))
 (52 52 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (51 51 (:REWRITE |(< (- x) (- y))|))
 (44 22 (:REWRITE BUBBLE-DOWN-+-MATCH-1))
 (43 31 (:REWRITE DEFAULT-UNARY-MINUS))
 (42 42 (:REWRITE |(+ c (+ d x))|))
 (38 38 (:REWRITE DEFAULT-CDR))
 (38 38 (:REWRITE |(+ x (- x))|))
 (32 32 (:REWRITE |(- (- x))|))
 (29 24 (:REWRITE DEFAULT-<-1))
 (27 24 (:REWRITE DEFAULT-<-2))
 (16 16 (:REWRITE |(< d (+ c x))|))
 (15 15 (:REWRITE |(< (+ c x) d)|))
 (12 12 (:REWRITE |(< 0 (- x))|))
 (10 5 (:DEFINITION TRUE-LISTP))
 (8 4 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (8 4 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (8 4 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (7 7 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-0-<-A+AB))
 (7 7 (:REWRITE FOLD-CONSTS-IN-+))
 (5 5 (:REWRITE DEFAULT-CAR))
 (4 4 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (4 4 (:REWRITE |(equal (- x) (- y))|))
 (4 4 (:REWRITE |(< (- x) 0)|))
 (4 4 (:REWRITE |(< (+ d x) (+ c y))|))
 (4 4 (:REWRITE |(< (+ c x) (+ d y))|))
 (4 4 (:REWRITE |(< (+ c x y) d)|))
 (3 3 (:REWRITE REDUCE-INTEGERP-+))
 (3 3 (:REWRITE INTEGERP-MINUS-X))
 (3 3 (:META META-INTEGERP-CORRECT))
 )
(COPY-FROM-STOBJ-ARRAY-NOOP
 (84 12 (:DEFINITION LEN))
 (24 12 (:REWRITE DEFAULT-+-2))
 (13 13 (:REWRITE DEFAULT-CDR))
 (12 12 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (12 12 (:REWRITE DEFAULT-+-1))
 (6 3 (:REWRITE DEFAULT-UNARY-MINUS))
 (2 2 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (2 2 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (2 2 (:REWRITE NORMALIZE-TERMS-SUCH-AS-1/AX+BX))
 (2 2 (:REWRITE DEFAULT-UNARY-/))
 (2 2 (:REWRITE |(< (- x) (- y))|))
 (2 1 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (2 1 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (2 1 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (2 1 (:DEFINITION TRUE-LISTP))
 (1 1 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-=-0))
 (1 1 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-0-<-A+AB))
 (1 1 (:REWRITE SIMPLIFY-SUMS-<))
 (1 1 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (1 1 (:REWRITE DEFAULT-<-2))
 (1 1 (:REWRITE DEFAULT-<-1))
 (1 1 (:REWRITE |(equal (- x) 0)|))
 (1 1 (:REWRITE |(equal (- x) (- y))|))
 (1 1 (:REWRITE |(< 0 (- x))|))
 )
(COPY-TO-STOBJ-ARRAY
 (16 8 (:TYPE-PRESCRIPTION TRUE-LISTP-UPDATE-NTH))
 (8 8 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (7 1 (:DEFINITION LEN))
 (6 6 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (6 6 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (6 5 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (5 5 (:REWRITE |(< (- x) (- y))|))
 (4 3 (:REWRITE DEFAULT-<-1))
 (4 3 (:REWRITE DEFAULT-+-2))
 (3 3 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (3 3 (:REWRITE DEFAULT-<-2))
 (3 3 (:REWRITE DEFAULT-+-1))
 (2 2 (:REWRITE DEFAULT-UNARY-MINUS))
 (1 1 (:REWRITE ZP-OPEN))
 (1 1 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (1 1 (:REWRITE DEFAULT-CDR))
 (1 1 (:REWRITE |(< d (+ c x))|))
 (1 1 (:REWRITE |(< (- x) 0)|))
 )
(CDR-COPY-TO-STOBJ-ARRAY
 (2026 34 (:REWRITE UPDATE-NTH-NTH-NOOP))
 (707 494 (:REWRITE DEFAULT-+-2))
 (659 8 (:REWRITE UPDATE-NTH-UPDATE-NTH-DIFF))
 (615 615 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (506 498 (:REWRITE NORMALIZE-ADDENDS))
 (494 494 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (494 494 (:REWRITE DEFAULT-+-1))
 (422 338 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (354 304 (:REWRITE SIMPLIFY-SUMS-<))
 (346 304 (:REWRITE DEFAULT-<-1))
 (338 338 (:REWRITE |(< (- x) (- y))|))
 (312 304 (:REWRITE DEFAULT-<-2))
 (165 144 (:REWRITE DEFAULT-CAR))
 (150 45 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (126 45 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (126 45 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (115 8 (:REWRITE CAR-UPDATE-NTH))
 (112 2 (:REWRITE NTH-UPDATE-NTH-2))
 (92 4 (:REWRITE |(< (+ c x) (+ d y))|))
 (88 7 (:REWRITE |(equal (if a b c) x)|))
 (45 45 (:REWRITE |(equal (- x) (- y))|))
 (44 2 (:REWRITE |(equal (+ c x) (+ d y))|))
 (37 9 (:REWRITE |(equal (+ c x) d)|))
 (16 4 (:REWRITE |(+ y x)|))
 (15 15 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (15 15 (:REWRITE |(< (- x) 0)|))
 (10 2 (:REWRITE |(+ y (+ x z))|))
 (8 8 (:REWRITE REDUCE-INTEGERP-+))
 (8 8 (:REWRITE INTEGERP-MINUS-X))
 (8 8 (:REWRITE CAR-CONS))
 (8 8 (:META META-INTEGERP-CORRECT))
 (8 4 (:REWRITE BUBBLE-DOWN-+-MATCH-1))
 (4 4 (:REWRITE DEFAULT-UNARY-MINUS))
 (4 4 (:REWRITE |(+ x (- x))|))
 (2 2 (:REWRITE |(equal (+ d x) (+ c y))|))
 )
(CAR-COPY-TO-STOBJ-ARRAY
 (224 4 (:REWRITE UPDATE-NTH-NTH-NOOP))
 (133 19 (:DEFINITION LEN))
 (73 73 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (70 50 (:REWRITE DEFAULT-+-2))
 (50 50 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (50 50 (:REWRITE NORMALIZE-ADDENDS))
 (50 50 (:REWRITE DEFAULT-+-1))
 (47 39 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (45 45 (:REWRITE DEFAULT-CDR))
 (45 38 (:REWRITE SIMPLIFY-SUMS-<))
 (45 38 (:REWRITE DEFAULT-<-1))
 (39 39 (:REWRITE |(< (- x) (- y))|))
 (38 38 (:REWRITE DEFAULT-<-2))
 (25 7 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (25 7 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (25 7 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (7 7 (:REWRITE |(equal (- x) (- y))|))
 (6 6 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-0-<-A+AB))
 (6 6 (:REWRITE REDUCE-INTEGERP-+))
 (6 6 (:REWRITE INTEGERP-MINUS-X))
 (6 6 (:REWRITE |(< 0 (- x))|))
 (6 6 (:META META-INTEGERP-CORRECT))
 )
(SUB1-CDR-CDR-INDUCTION)
(COPY-TO-STOBJ-ARRAY-FIRSTN
 (148 91 (:REWRITE DEFAULT-+-2))
 (105 103 (:REWRITE DEFAULT-CDR))
 (91 91 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (91 91 (:REWRITE NORMALIZE-ADDENDS))
 (91 91 (:REWRITE DEFAULT-+-1))
 (34 34 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (31 21 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (29 20 (:REWRITE DEFAULT-<-1))
 (27 20 (:REWRITE SIMPLIFY-SUMS-<))
 (26 24 (:REWRITE DEFAULT-CAR))
 (25 10 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (25 10 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (25 10 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (21 21 (:REWRITE |(< (- x) (- y))|))
 (20 20 (:REWRITE DEFAULT-<-2))
 (14 8 (:REWRITE |(< (+ c x) d)|))
 (10 10 (:REWRITE |(equal (- x) (- y))|))
 (9 3 (:REWRITE |(equal (+ d x) (+ c y))|))
 (9 3 (:REWRITE |(equal (+ c x) (+ d y))|))
 (2 2 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-=-0))
 (2 2 (:REWRITE |(equal (- x) 0)|))
 (1 1 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (1 1 (:REWRITE |(< (- x) 0)|))
 )
(FIRSTN-WITH-LARGE-INDEX
 (28 18 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (27 16 (:REWRITE DEFAULT-+-2))
 (26 17 (:REWRITE SIMPLIFY-SUMS-<))
 (26 17 (:REWRITE DEFAULT-<-2))
 (25 25 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (19 19 (:REWRITE DEFAULT-CDR))
 (18 18 (:REWRITE |(< (- x) (- y))|))
 (17 17 (:REWRITE DEFAULT-<-1))
 (16 16 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (16 16 (:REWRITE NORMALIZE-ADDENDS))
 (16 16 (:REWRITE DEFAULT-+-1))
 (14 7 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (14 7 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (14 7 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (8 8 (:REWRITE REDUCE-INTEGERP-+))
 (8 8 (:REWRITE INTEGERP-MINUS-X))
 (8 8 (:META META-INTEGERP-CORRECT))
 (7 7 (:REWRITE |(equal (- x) (- y))|))
 (6 6 (:REWRITE ZP-OPEN))
 (6 6 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (6 6 (:REWRITE |(< (- x) 0)|))
 (5 5 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-=-0))
 (2 2 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-0-<-A+AB))
 (2 2 (:REWRITE |(equal (- x) 0)|))
 (2 2 (:REWRITE |(< 0 (- x))|))
 )
(TRUE-LISTP-COPY-TO-STOBJ-ARRAY
 (88 8 (:DEFINITION TRUE-LISTP))
 (84 22 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (84 3 (:REWRITE UPDATE-NTH-NTH-NOOP))
 (83 11 (:REWRITE ZP-OPEN))
 (70 10 (:DEFINITION LEN))
 (66 3 (:DEFINITION UPDATE-NTH))
 (63 3 (:DEFINITION NTH))
 (59 3 (:REWRITE CDR-COPY-TO-STOBJ-ARRAY))
 (58 10 (:REWRITE |(< d (+ c x))|))
 (40 11 (:REWRITE |(+ c (+ d x))|))
 (33 23 (:REWRITE DEFAULT-+-2))
 (31 1 (:REWRITE LEN-UPDATE-NTH-BETTER))
 (30 27 (:REWRITE DEFAULT-CDR))
 (29 29 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (27 22 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (23 23 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (23 23 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (23 23 (:REWRITE NORMALIZE-ADDENDS))
 (23 23 (:REWRITE DEFAULT-+-1))
 (23 18 (:REWRITE SIMPLIFY-SUMS-<))
 (23 18 (:REWRITE DEFAULT-<-1))
 (22 22 (:REWRITE |(< (- x) (- y))|))
 (18 18 (:REWRITE DEFAULT-<-2))
 (13 5 (:REWRITE |(< (+ c x) d)|))
 (12 1 (:DEFINITION NFIX))
 (9 1 (:DEFINITION MAX))
 (6 6 (:REWRITE DEFAULT-CAR))
 (5 5 (:REWRITE |(+ 0 x)|))
 )
(LEN-COPY-TO-STOBJ-ARRAY
 (189 9 (:DEFINITION NTH))
 (166 3 (:REWRITE UPDATE-NTH-NTH-NOOP))
 (164 20 (:REWRITE ZP-OPEN))
 (111 15 (:REWRITE |(< d (+ c x))|))
 (80 4 (:REWRITE CDR-COPY-TO-STOBJ-ARRAY))
 (73 46 (:REWRITE DEFAULT-+-2))
 (66 3 (:DEFINITION UPDATE-NTH))
 (48 40 (:REWRITE DEFAULT-CDR))
 (47 47 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (46 46 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (46 46 (:REWRITE NORMALIZE-ADDENDS))
 (46 46 (:REWRITE DEFAULT-+-1))
 (34 25 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (29 22 (:REWRITE SIMPLIFY-SUMS-<))
 (29 22 (:REWRITE DEFAULT-<-1))
 (25 25 (:REWRITE |(< (- x) (- y))|))
 (22 22 (:REWRITE DEFAULT-<-2))
 (12 12 (:REWRITE DEFAULT-CAR))
 (11 6 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (11 6 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (11 6 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (6 6 (:REWRITE |(equal (- x) (- y))|))
 )
(COPY-TO-STOBJ-ARRAY-NOOP
 (24 12 (:REWRITE DEFAULT-+-2))
 (12 12 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (12 12 (:REWRITE NORMALIZE-ADDENDS))
 (12 12 (:REWRITE DEFAULT-CDR))
 (12 12 (:REWRITE DEFAULT-+-1))
 (5 3 (:REWRITE SIMPLIFY-SUMS-<))
 (5 3 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (5 3 (:REWRITE DEFAULT-<-1))
 (4 4 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (4 4 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (4 4 (:REWRITE |(< (- x) (- y))|))
 (3 3 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (2 2 (:REWRITE NORMALIZE-TERMS-SUCH-AS-1/AX+BX))
 (2 2 (:REWRITE DEFAULT-UNARY-/))
 (2 2 (:REWRITE |(< (- x) 0)|))
 (2 1 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (2 1 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (2 1 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (1 1 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-=-0))
 (1 1 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-0-<-A+AB))
 (1 1 (:REWRITE |(equal (- x) 0)|))
 (1 1 (:REWRITE |(equal (- x) (- y))|))
 (1 1 (:REWRITE |(< 0 (- x))|))
 )
(SUB1-CDR-CDR-CDR-INDUCTION)
(COPY-TO-STOBJ-ARRAY-IGNORES-SECOND-ARG-L1
 (1676 22 (:REWRITE FIRSTN-WITH-LARGE-INDEX))
 (706 16 (:REWRITE LEN-COPY-TO-STOBJ-ARRAY))
 (461 16 (:REWRITE COPY-TO-STOBJ-ARRAY-NOOP))
 (338 183 (:REWRITE DEFAULT-+-2))
 (236 20 (:DEFINITION NFIX))
 (227 30 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (192 58 (:REWRITE |(< (+ c x) d)|))
 (183 183 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (183 183 (:REWRITE NORMALIZE-ADDENDS))
 (183 183 (:REWRITE DEFAULT-+-1))
 (180 105 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (152 150 (:REWRITE DEFAULT-CDR))
 (133 133 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (105 105 (:REWRITE |(< (- x) (- y))|))
 (105 78 (:REWRITE DEFAULT-<-1))
 (100 78 (:REWRITE DEFAULT-<-2))
 (97 78 (:REWRITE SIMPLIFY-SUMS-<))
 (88 37 (:REWRITE |(< d (+ c x))|))
 (79 7 (:REWRITE ZP-OPEN))
 (64 30 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (46 21 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (30 30 (:REWRITE |(equal (- x) (- y))|))
 (27 22 (:REWRITE |(equal (+ c x) d)|))
 (22 22 (:TYPE-PRESCRIPTION NFIX))
 (18 16 (:REWRITE DEFAULT-CAR))
 (12 12 (:REWRITE COPY-TO-STOBJ-ARRAY-FIRSTN))
 (6 2 (:REWRITE |(equal (+ d x) (+ c y))|))
 (6 2 (:REWRITE |(equal (+ c x) (+ d y))|))
 (5 5 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (5 5 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-0-<-A+AB))
 (5 5 (:REWRITE |(< 0 (- x))|))
 (5 5 (:REWRITE |(< (- x) 0)|))
 (1 1 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-=-0))
 (1 1 (:REWRITE |(equal (- x) 0)|))
 )
(COPY-TO-STOBJ-ARRAY-IGNORES-SECOND-ARG
 (30 15 (:REWRITE DEFAULT-+-2))
 (30 2 (:REWRITE COPY-TO-STOBJ-ARRAY-NOOP))
 (15 15 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (15 15 (:REWRITE NORMALIZE-ADDENDS))
 (15 15 (:REWRITE DEFAULT-CDR))
 (15 15 (:REWRITE DEFAULT-+-1))
 (5 3 (:REWRITE SIMPLIFY-SUMS-<))
 (5 3 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (5 3 (:REWRITE DEFAULT-<-1))
 (5 2 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (5 2 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (5 2 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (4 4 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (4 4 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (4 4 (:REWRITE |(< (- x) (- y))|))
 (3 3 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-<-0))
 (2 2 (:REWRITE NORMALIZE-TERMS-SUCH-AS-1/AX+BX))
 (2 2 (:REWRITE DEFAULT-UNARY-/))
 (2 2 (:REWRITE |(equal (- x) (- y))|))
 (2 2 (:REWRITE |(< (- x) 0)|))
 (1 1 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-=-0))
 (1 1 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-0-<-A+AB))
 (1 1 (:REWRITE |(equal (- x) 0)|))
 (1 1 (:REWRITE |(< 0 (- x))|))
 )
(LEN-0-ATOM
 (9 5 (:REWRITE DEFAULT-+-2))
 (8 4 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (8 4 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (8 4 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (5 5 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (5 5 (:REWRITE NORMALIZE-ADDENDS))
 (5 5 (:REWRITE DEFAULT-+-1))
 (4 4 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-=-0))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 4 (:REWRITE |(equal (- x) 0)|))
 (4 4 (:REWRITE |(equal (- x) (- y))|))
 )
(CONS-EQUAL-CAR
 (6 6 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (6 6 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (6 6 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (6 6 (:REWRITE |(equal (- x) (- y))|))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 4 (:REWRITE DEFAULT-CAR))
 )
(LEN>0-CONSP)
(ADD-CANCEL
 (8 8 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (8 8 (:REWRITE DEFAULT-+-2))
 (8 8 (:REWRITE DEFAULT-+-1))
 (5 5 (:REWRITE DEFAULT-UNARY-MINUS))
 (5 5 (:REWRITE |(+ c (+ d x))|))
 (4 4 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (4 4 (:REWRITE |(equal (- x) (- y))|))
 (4 4 (:REWRITE |(equal (+ c x) d)|))
 (1 1 (:REWRITE |(equal (+ d x) (+ c y))|))
 (1 1 (:REWRITE |(equal (+ c x) (+ d y))|))
 )
(SYMBOL-LIST-TO-STRING
 (2 2 (:REWRITE DEFAULT-CAR))
 (1 1 (:REWRITE DEFAULT-CDR))
 )
(GENERATE-ARRAY-THEOREMS)
(GET-TYPE-FROM-FIELD-DECLARATION)
(GENERATE-DEFSTOBJ+-AUX)
(GENERATE-DEFSTOBJ+)
