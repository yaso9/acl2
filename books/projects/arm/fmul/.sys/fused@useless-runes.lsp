(RTL::FUSED-10
 (214 8 (:REWRITE RTL::NEG-BITN-0))
 (158 8 (:REWRITE RTL::NEG-BITN-1))
 (52 2 (:REWRITE RTL::OPAZ-OPA))
 (27 6 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (24 8 (:REWRITE RTL::BVECP-BITN-0))
 (18 2 (:REWRITE ACL2-NUMBERP-X))
 (16 16 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-ODD-EXPONENT))
 (16 16 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-EVEN-EXPONENT))
 (16 16 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-ODD-EXPONENT))
 (16 16 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-EVEN-EXPONENT))
 (16 16 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-B))
 (16 16 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-A))
 (16 8 (:REWRITE DEFAULT-LESS-THAN-1))
 (14 8 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (14 8 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (13 6 (:REWRITE EQUAL-OF-BOOLEANS-REWRITE))
 (8 8 (:REWRITE THE-FLOOR-BELOW))
 (8 8 (:REWRITE THE-FLOOR-ABOVE))
 (8 8 (:REWRITE SIMPLIFY-SUMS-<))
 (8 8 (:REWRITE REMOVE-STRICT-INEQUALITIES))
 (8 8 (:REWRITE REDUCE-RATIONAL-MULTIPLICATIVE-CONSTANT-<))
 (8 8 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-<))
 (8 8 (:REWRITE INTEGERP-<-CONSTANT))
 (8 8 (:REWRITE DEFAULT-LESS-THAN-2))
 (8 8 (:REWRITE CONSTANT-<-INTEGERP))
 (8 8 (:REWRITE RTL::BITN-NEG))
 (8 8 (:REWRITE |(< c (/ x)) positive c --- present in goal|))
 (8 8 (:REWRITE |(< c (/ x)) positive c --- obj t or nil|))
 (8 8 (:REWRITE |(< c (/ x)) negative c --- present in goal|))
 (8 8 (:REWRITE |(< c (/ x)) negative c --- obj t or nil|))
 (8 8 (:REWRITE |(< c (- x))|))
 (8 8 (:REWRITE |(< (/ x) c) positive c --- present in goal|))
 (8 8 (:REWRITE |(< (/ x) c) positive c --- obj t or nil|))
 (8 8 (:REWRITE |(< (/ x) c) negative c --- present in goal|))
 (8 8 (:REWRITE |(< (/ x) c) negative c --- obj t or nil|))
 (8 8 (:REWRITE |(< (/ x) (/ y))|))
 (8 8 (:REWRITE |(< (- x) c)|))
 (8 8 (:REWRITE |(< (- x) (- y))|))
 (8 2 (:REWRITE RATIONALP-X))
 (6 6 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (6 6 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-EQUAL))
 (6 6 (:REWRITE REDUCE-ADDITIVE-CONSTANT-EQUAL))
 (6 6 (:REWRITE EQUAL-OF-PREDICATES-REWRITE))
 (6 6 (:REWRITE |(equal c (/ x))|))
 (6 6 (:REWRITE |(equal c (- x))|))
 (6 6 (:REWRITE |(equal (/ x) c)|))
 (6 6 (:REWRITE |(equal (/ x) (/ y))|))
 (6 6 (:REWRITE |(equal (- x) c)|))
 (6 6 (:REWRITE |(equal (- x) (- y))|))
 (4 4 (:TYPE-PRESCRIPTION BOOLEANP))
 (4 4 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-<-0-RATIONAL-REMAINDER))
 (4 4 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-<-0-RATIONAL-COMMON))
 (4 4 (:REWRITE |(< (/ x) 0)|))
 (4 4 (:REWRITE |(< (* x y) 0)|))
 (3 3 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-=-0))
 (2 2 (:REWRITE REDUCE-RATIONALP-+))
 (2 2 (:REWRITE REDUCE-RATIONALP-*))
 (2 2 (:REWRITE REDUCE-INTEGERP-+))
 (2 2 (:REWRITE RATIONALP-MINUS-X))
 (2 2 (:REWRITE INTEGERP-MINUS-X))
 (2 2 (:META META-RATIONALP-CORRECT))
 (2 2 (:META META-INTEGERP-CORRECT))
 )
(RTL::FUSED-11
 (214 8 (:REWRITE RTL::NEG-BITN-0))
 (158 8 (:REWRITE RTL::NEG-BITN-1))
 (52 2 (:REWRITE RTL::OPBZ-OPB))
 (27 6 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (24 8 (:REWRITE RTL::BVECP-BITN-0))
 (18 2 (:REWRITE ACL2-NUMBERP-X))
 (16 16 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-ODD-EXPONENT))
 (16 16 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-EVEN-EXPONENT))
 (16 16 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-ODD-EXPONENT))
 (16 16 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-EVEN-EXPONENT))
 (16 16 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-B))
 (16 16 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-A))
 (16 8 (:REWRITE DEFAULT-LESS-THAN-1))
 (14 8 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (14 8 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (13 6 (:REWRITE EQUAL-OF-BOOLEANS-REWRITE))
 (8 8 (:REWRITE THE-FLOOR-BELOW))
 (8 8 (:REWRITE THE-FLOOR-ABOVE))
 (8 8 (:REWRITE SIMPLIFY-SUMS-<))
 (8 8 (:REWRITE REMOVE-STRICT-INEQUALITIES))
 (8 8 (:REWRITE REDUCE-RATIONAL-MULTIPLICATIVE-CONSTANT-<))
 (8 8 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-<))
 (8 8 (:REWRITE INTEGERP-<-CONSTANT))
 (8 8 (:REWRITE DEFAULT-LESS-THAN-2))
 (8 8 (:REWRITE CONSTANT-<-INTEGERP))
 (8 8 (:REWRITE RTL::BITN-NEG))
 (8 8 (:REWRITE |(< c (/ x)) positive c --- present in goal|))
 (8 8 (:REWRITE |(< c (/ x)) positive c --- obj t or nil|))
 (8 8 (:REWRITE |(< c (/ x)) negative c --- present in goal|))
 (8 8 (:REWRITE |(< c (/ x)) negative c --- obj t or nil|))
 (8 8 (:REWRITE |(< c (- x))|))
 (8 8 (:REWRITE |(< (/ x) c) positive c --- present in goal|))
 (8 8 (:REWRITE |(< (/ x) c) positive c --- obj t or nil|))
 (8 8 (:REWRITE |(< (/ x) c) negative c --- present in goal|))
 (8 8 (:REWRITE |(< (/ x) c) negative c --- obj t or nil|))
 (8 8 (:REWRITE |(< (/ x) (/ y))|))
 (8 8 (:REWRITE |(< (- x) c)|))
 (8 8 (:REWRITE |(< (- x) (- y))|))
 (8 2 (:REWRITE RATIONALP-X))
 (6 6 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (6 6 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-EQUAL))
 (6 6 (:REWRITE REDUCE-ADDITIVE-CONSTANT-EQUAL))
 (6 6 (:REWRITE EQUAL-OF-PREDICATES-REWRITE))
 (6 6 (:REWRITE |(equal c (/ x))|))
 (6 6 (:REWRITE |(equal c (- x))|))
 (6 6 (:REWRITE |(equal (/ x) c)|))
 (6 6 (:REWRITE |(equal (/ x) (/ y))|))
 (6 6 (:REWRITE |(equal (- x) c)|))
 (6 6 (:REWRITE |(equal (- x) (- y))|))
 (4 4 (:TYPE-PRESCRIPTION BOOLEANP))
 (4 4 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-<-0-RATIONAL-REMAINDER))
 (4 4 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-<-0-RATIONAL-COMMON))
 (4 4 (:REWRITE |(< (/ x) 0)|))
 (4 4 (:REWRITE |(< (* x y) 0)|))
 (3 3 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-=-0))
 (2 2 (:REWRITE REDUCE-RATIONALP-+))
 (2 2 (:REWRITE REDUCE-RATIONALP-*))
 (2 2 (:REWRITE REDUCE-INTEGERP-+))
 (2 2 (:REWRITE RATIONALP-MINUS-X))
 (2 2 (:REWRITE INTEGERP-MINUS-X))
 (2 2 (:META META-RATIONALP-CORRECT))
 (2 2 (:META META-INTEGERP-CORRECT))
 )
(RTL::FUSED-12
 (21267 2806 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (19782 2198 (:REWRITE ACL2-NUMBERP-X))
 (18747 738 (:REWRITE |(< (+ (- c) x) y)|))
 (14601 669 (:REWRITE RTL::NEG-BITN-0))
 (12512 297 (:DEFINITION NATP))
 (8794 669 (:REWRITE RTL::NEG-BITN-1))
 (8792 2198 (:REWRITE RATIONALP-X))
 (7246 2806 (:REWRITE EQUAL-OF-BOOLEANS-REWRITE))
 (4006 4006 (:TYPE-PRESCRIPTION BOOLEANP))
 (3626 518 (:REWRITE INTEGERP-+-REDUCE-CONSTANT))
 (3540 103 (:REWRITE |(< (if a b c) x)|))
 (3329 3329 (:REWRITE REDUCE-INTEGERP-+))
 (3329 3329 (:REWRITE INTEGERP-MINUS-X))
 (3329 3329 (:META META-INTEGERP-CORRECT))
 (2806 2806 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (2806 2806 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-EQUAL))
 (2806 2806 (:REWRITE REDUCE-ADDITIVE-CONSTANT-EQUAL))
 (2806 2806 (:REWRITE EQUAL-OF-PREDICATES-REWRITE))
 (2806 2806 (:REWRITE |(equal c (/ x))|))
 (2806 2806 (:REWRITE |(equal c (- x))|))
 (2806 2806 (:REWRITE |(equal (/ x) c)|))
 (2806 2806 (:REWRITE |(equal (/ x) (/ y))|))
 (2806 2806 (:REWRITE |(equal (- x) c)|))
 (2806 2806 (:REWRITE |(equal (- x) (- y))|))
 (2198 2198 (:REWRITE REDUCE-RATIONALP-+))
 (2198 2198 (:REWRITE REDUCE-RATIONALP-*))
 (2198 2198 (:REWRITE RATIONALP-MINUS-X))
 (2198 2198 (:META META-RATIONALP-CORRECT))
 (2007 669 (:REWRITE RTL::BVECP-BITN-0))
 (1869 1724 (:REWRITE DEFAULT-LESS-THAN-1))
 (1724 1724 (:REWRITE THE-FLOOR-BELOW))
 (1724 1724 (:REWRITE THE-FLOOR-ABOVE))
 (1724 1724 (:REWRITE DEFAULT-LESS-THAN-2))
 (1621 1621 (:REWRITE REDUCE-RATIONAL-MULTIPLICATIVE-CONSTANT-<))
 (1621 1621 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-<))
 (1028 883 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (1028 883 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (959 959 (:REWRITE DEFAULT-PLUS-1))
 (925 925 (:REWRITE SUBSETP-MEMBER . 4))
 (925 925 (:REWRITE SUBSETP-MEMBER . 3))
 (925 925 (:REWRITE SUBSETP-MEMBER . 2))
 (925 925 (:REWRITE SUBSETP-MEMBER . 1))
 (925 925 (:REWRITE INTERSECTP-MEMBER . 3))
 (925 925 (:REWRITE INTERSECTP-MEMBER . 2))
 (883 883 (:REWRITE SIMPLIFY-SUMS-<))
 (883 883 (:REWRITE REMOVE-STRICT-INEQUALITIES))
 (883 883 (:REWRITE INTEGERP-<-CONSTANT))
 (883 883 (:REWRITE CONSTANT-<-INTEGERP))
 (883 883 (:REWRITE |(< c (/ x)) positive c --- present in goal|))
 (883 883 (:REWRITE |(< c (/ x)) positive c --- obj t or nil|))
 (883 883 (:REWRITE |(< c (/ x)) negative c --- present in goal|))
 (883 883 (:REWRITE |(< c (/ x)) negative c --- obj t or nil|))
 (883 883 (:REWRITE |(< c (- x))|))
 (883 883 (:REWRITE |(< (/ x) c) positive c --- present in goal|))
 (883 883 (:REWRITE |(< (/ x) c) positive c --- obj t or nil|))
 (883 883 (:REWRITE |(< (/ x) c) negative c --- present in goal|))
 (883 883 (:REWRITE |(< (/ x) c) negative c --- obj t or nil|))
 (883 883 (:REWRITE |(< (/ x) (/ y))|))
 (883 883 (:REWRITE |(< (- x) c)|))
 (883 883 (:REWRITE |(< (- x) (- y))|))
 (883 883 (:REWRITE |(< (* x y) 0)|))
 (738 738 (:REWRITE |(< (+ c/d x) y)|))
 (518 518 (:REWRITE |(+ 0 x)|))
 (486 486 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-=-0))
 (441 441 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (441 441 (:REWRITE NORMALIZE-ADDENDS))
 (297 297 (:TYPE-PRESCRIPTION NATP))
 (297 297 (:REWRITE REMOVE-WEAK-INEQUALITIES))
 (145 145 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-<-0-RATIONAL-REMAINDER))
 (145 145 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-<-0-RATIONAL-COMMON))
 (145 145 (:REWRITE |(< (/ x) 0)|))
 )
(RTL::FUSED-13
 (3059 342 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (2700 300 (:REWRITE ACL2-NUMBERP-X))
 (1200 300 (:REWRITE RATIONALP-X))
 (959 342 (:REWRITE EQUAL-OF-BOOLEANS-REWRITE))
 (600 600 (:TYPE-PRESCRIPTION BOOLEANP))
 (342 342 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (342 342 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-EQUAL))
 (342 342 (:REWRITE REDUCE-ADDITIVE-CONSTANT-EQUAL))
 (342 342 (:REWRITE EQUAL-OF-PREDICATES-REWRITE))
 (342 342 (:REWRITE |(equal c (/ x))|))
 (342 342 (:REWRITE |(equal c (- x))|))
 (342 342 (:REWRITE |(equal (/ x) c)|))
 (342 342 (:REWRITE |(equal (/ x) (/ y))|))
 (342 342 (:REWRITE |(equal (- x) c)|))
 (342 342 (:REWRITE |(equal (- x) (- y))|))
 (300 300 (:REWRITE REDUCE-RATIONALP-+))
 (300 300 (:REWRITE REDUCE-RATIONALP-*))
 (300 300 (:REWRITE REDUCE-INTEGERP-+))
 (300 300 (:REWRITE RATIONALP-MINUS-X))
 (300 300 (:REWRITE INTEGERP-MINUS-X))
 (300 300 (:META META-RATIONALP-CORRECT))
 (300 300 (:META META-INTEGERP-CORRECT))
 (140 140 (:REWRITE SUBSETP-MEMBER . 4))
 (140 140 (:REWRITE SUBSETP-MEMBER . 3))
 (140 140 (:REWRITE SUBSETP-MEMBER . 2))
 (140 140 (:REWRITE SUBSETP-MEMBER . 1))
 (140 140 (:REWRITE INTERSECTP-MEMBER . 3))
 (140 140 (:REWRITE INTERSECTP-MEMBER . 2))
 (132 11 (:REWRITE RTL::NEG-BITN-0))
 (67 67 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-=-0))
 (33 11 (:REWRITE RTL::NEG-BITN-1))
 (33 11 (:REWRITE RTL::BVECP-BITN-0))
 (15 15 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-ODD-EXPONENT))
 (15 15 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-EVEN-EXPONENT))
 (15 15 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-ODD-EXPONENT))
 (15 15 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-EVEN-EXPONENT))
 (15 15 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-B))
 (15 15 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-A))
 (11 11 (:REWRITE RTL::BITN-NEG))
 )
(RTL::FUSED-14
 (3047 336 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (2700 300 (:REWRITE ACL2-NUMBERP-X))
 (1200 300 (:REWRITE RATIONALP-X))
 (947 336 (:REWRITE EQUAL-OF-BOOLEANS-REWRITE))
 (732 61 (:REWRITE RTL::NEG-BITN-0))
 (600 600 (:TYPE-PRESCRIPTION BOOLEANP))
 (336 336 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (336 336 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-EQUAL))
 (336 336 (:REWRITE REDUCE-ADDITIVE-CONSTANT-EQUAL))
 (336 336 (:REWRITE EQUAL-OF-PREDICATES-REWRITE))
 (336 336 (:REWRITE |(equal c (/ x))|))
 (336 336 (:REWRITE |(equal c (- x))|))
 (336 336 (:REWRITE |(equal (/ x) c)|))
 (336 336 (:REWRITE |(equal (/ x) (/ y))|))
 (336 336 (:REWRITE |(equal (- x) c)|))
 (336 336 (:REWRITE |(equal (- x) (- y))|))
 (300 300 (:REWRITE REDUCE-RATIONALP-+))
 (300 300 (:REWRITE REDUCE-RATIONALP-*))
 (300 300 (:REWRITE REDUCE-INTEGERP-+))
 (300 300 (:REWRITE RATIONALP-MINUS-X))
 (300 300 (:REWRITE INTEGERP-MINUS-X))
 (300 300 (:META META-RATIONALP-CORRECT))
 (300 300 (:META META-INTEGERP-CORRECT))
 (183 61 (:REWRITE RTL::NEG-BITN-1))
 (140 140 (:REWRITE SUBSETP-MEMBER . 4))
 (140 140 (:REWRITE SUBSETP-MEMBER . 3))
 (140 140 (:REWRITE SUBSETP-MEMBER . 2))
 (140 140 (:REWRITE SUBSETP-MEMBER . 1))
 (140 140 (:REWRITE INTERSECTP-MEMBER . 3))
 (140 140 (:REWRITE INTERSECTP-MEMBER . 2))
 (107 61 (:REWRITE RTL::BVECP-BITN-0))
 (83 83 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-ODD-EXPONENT))
 (83 83 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-EVEN-EXPONENT))
 (83 83 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-ODD-EXPONENT))
 (83 83 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-EVEN-EXPONENT))
 (83 83 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-B))
 (83 83 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-A))
 (61 61 (:REWRITE RTL::BITN-NEG))
 (53 53 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-=-0))
 )
(RTL::FUSED-15
 (1071 7 (:REWRITE RTL::CASE-2-REWRITE))
 (1064 7 (:REWRITE RTL::CASE-1-10))
 (511 7 (:REWRITE RTL::FINAL-7))
 (290 290 (:REWRITE SUBSETP-MEMBER . 4))
 (290 290 (:REWRITE SUBSETP-MEMBER . 3))
 (290 290 (:REWRITE SUBSETP-MEMBER . 2))
 (290 290 (:REWRITE SUBSETP-MEMBER . 1))
 (290 290 (:REWRITE INTERSECTP-MEMBER . 3))
 (290 290 (:REWRITE INTERSECTP-MEMBER . 2))
 (210 14 (:REWRITE |(+ y (+ x z))|))
 (161 161 (:TYPE-PRESCRIPTION RTL::INTEGERP-EB))
 (140 70 (:REWRITE DEFAULT-PLUS-2))
 (119 119 (:TYPE-PRESCRIPTION RTL::INTEGERP-EA))
 (112 70 (:REWRITE DEFAULT-PLUS-1))
 (90 9 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (84 14 (:REWRITE |(+ y x)|))
 (81 9 (:REWRITE ACL2-NUMBERP-X))
 (42 42 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (42 42 (:REWRITE NORMALIZE-ADDENDS))
 (42 14 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (42 14 (:REWRITE DEFAULT-LESS-THAN-2))
 (36 9 (:REWRITE RATIONALP-X))
 (28 14 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (28 7 (:REWRITE RTL::NEG-BITN-1))
 (28 7 (:REWRITE RTL::NEG-BITN-0))
 (27 9 (:REWRITE EQUAL-OF-BOOLEANS-REWRITE))
 (23 23 (:REWRITE REDUCE-INTEGERP-+))
 (23 23 (:REWRITE INTEGERP-MINUS-X))
 (23 23 (:META META-INTEGERP-CORRECT))
 (21 7 (:REWRITE RTL::BVECP-BITN-0))
 (21 7 (:REWRITE RTL::BITN-BVECP-1))
 (18 18 (:TYPE-PRESCRIPTION BOOLEANP))
 (14 14 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-POSITIVE-BASE))
 (14 14 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-ODD-EXPONENT))
 (14 14 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-EVEN-EXPONENT))
 (14 14 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONNEGATIVE-BASE))
 (14 14 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-ODD-EXPONENT))
 (14 14 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-EVEN-EXPONENT))
 (14 14 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-B))
 (14 14 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-A))
 (14 14 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE))
 (14 14 (:REWRITE THE-FLOOR-BELOW))
 (14 14 (:REWRITE THE-FLOOR-ABOVE))
 (14 14 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-0-<-AX+BX-RATIONAL-REMAINDER))
 (14 14 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-0-<-AX+BX-RATIONAL-COMMON))
 (14 14 (:REWRITE SIMPLIFY-SUMS-<))
 (14 14 (:REWRITE REMOVE-STRICT-INEQUALITIES))
 (14 14 (:REWRITE REDUCE-RATIONAL-MULTIPLICATIVE-CONSTANT-<))
 (14 14 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-<))
 (14 14 (:REWRITE INTEGERP-<-CONSTANT))
 (14 14 (:REWRITE FOLD-CONSTS-IN-+))
 (14 14 (:REWRITE DEFAULT-LESS-THAN-1))
 (14 14 (:REWRITE CONSTANT-<-INTEGERP))
 (14 14 (:REWRITE |(< y (+ (- c) x))|))
 (14 14 (:REWRITE |(< x (+ c/d y))|))
 (14 14 (:REWRITE |(< c (/ x)) positive c --- present in goal|))
 (14 14 (:REWRITE |(< c (/ x)) positive c --- obj t or nil|))
 (14 14 (:REWRITE |(< c (/ x)) negative c --- present in goal|))
 (14 14 (:REWRITE |(< c (/ x)) negative c --- obj t or nil|))
 (14 14 (:REWRITE |(< c (- x))|))
 (14 14 (:REWRITE |(< 0 (/ x))|))
 (14 14 (:REWRITE |(< 0 (* x y))|))
 (14 14 (:REWRITE |(< (/ x) c) positive c --- present in goal|))
 (14 14 (:REWRITE |(< (/ x) c) positive c --- obj t or nil|))
 (14 14 (:REWRITE |(< (/ x) c) negative c --- present in goal|))
 (14 14 (:REWRITE |(< (/ x) c) negative c --- obj t or nil|))
 (14 14 (:REWRITE |(< (/ x) (/ y))|))
 (14 14 (:REWRITE |(< (- x) c)|))
 (14 14 (:REWRITE |(< (- x) (- y))|))
 (14 14 (:REWRITE |(+ c (+ d x))|))
 (9 9 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (9 9 (:REWRITE REDUCE-RATIONALP-+))
 (9 9 (:REWRITE REDUCE-RATIONALP-*))
 (9 9 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-EQUAL))
 (9 9 (:REWRITE REDUCE-ADDITIVE-CONSTANT-EQUAL))
 (9 9 (:REWRITE RATIONALP-MINUS-X))
 (9 9 (:REWRITE EQUAL-OF-PREDICATES-REWRITE))
 (9 9 (:REWRITE |(equal c (/ x))|))
 (9 9 (:REWRITE |(equal c (- x))|))
 (9 9 (:REWRITE |(equal (/ x) c)|))
 (9 9 (:REWRITE |(equal (/ x) (/ y))|))
 (9 9 (:REWRITE |(equal (- x) c)|))
 (9 9 (:REWRITE |(equal (- x) (- y))|))
 (9 9 (:META META-RATIONALP-CORRECT))
 (7 7 (:REWRITE REMOVE-WEAK-INEQUALITIES))
 (7 7 (:REWRITE RTL::BITN-NEG))
 (2 2 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-=-0))
 )
(RTL::FUSED-16
 (90 9 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (81 9 (:REWRITE ACL2-NUMBERP-X))
 (36 9 (:REWRITE RATIONALP-X))
 (27 9 (:REWRITE EQUAL-OF-BOOLEANS-REWRITE))
 (18 18 (:TYPE-PRESCRIPTION BOOLEANP))
 (10 10 (:REWRITE SUBSETP-MEMBER . 4))
 (10 10 (:REWRITE SUBSETP-MEMBER . 3))
 (10 10 (:REWRITE SUBSETP-MEMBER . 2))
 (10 10 (:REWRITE SUBSETP-MEMBER . 1))
 (10 10 (:REWRITE INTERSECTP-MEMBER . 3))
 (10 10 (:REWRITE INTERSECTP-MEMBER . 2))
 (9 9 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (9 9 (:REWRITE REDUCE-RATIONALP-+))
 (9 9 (:REWRITE REDUCE-RATIONALP-*))
 (9 9 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-EQUAL))
 (9 9 (:REWRITE REDUCE-INTEGERP-+))
 (9 9 (:REWRITE REDUCE-ADDITIVE-CONSTANT-EQUAL))
 (9 9 (:REWRITE RATIONALP-MINUS-X))
 (9 9 (:REWRITE INTEGERP-MINUS-X))
 (9 9 (:REWRITE EQUAL-OF-PREDICATES-REWRITE))
 (9 9 (:REWRITE |(equal c (/ x))|))
 (9 9 (:REWRITE |(equal c (- x))|))
 (9 9 (:REWRITE |(equal (/ x) c)|))
 (9 9 (:REWRITE |(equal (/ x) (/ y))|))
 (9 9 (:REWRITE |(equal (- x) c)|))
 (9 9 (:REWRITE |(equal (- x) (- y))|))
 (9 9 (:META META-RATIONALP-CORRECT))
 (9 9 (:META META-INTEGERP-CORRECT))
 (2 2 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-=-0))
 )
(RTL::EXPOVFL-0-1
 (459 3 (:REWRITE RTL::CASE-2-REWRITE))
 (456 3 (:REWRITE RTL::CASE-1-10))
 (123 17 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (99 11 (:REWRITE ACL2-NUMBERP-X))
 (90 6 (:REWRITE |(+ y (+ x z))|))
 (70 70 (:REWRITE SUBSETP-MEMBER . 4))
 (70 70 (:REWRITE SUBSETP-MEMBER . 3))
 (70 70 (:REWRITE SUBSETP-MEMBER . 2))
 (70 70 (:REWRITE SUBSETP-MEMBER . 1))
 (70 70 (:REWRITE INTERSECTP-MEMBER . 3))
 (70 70 (:REWRITE INTERSECTP-MEMBER . 2))
 (69 69 (:TYPE-PRESCRIPTION RTL::INTEGERP-EB))
 (60 30 (:REWRITE DEFAULT-PLUS-2))
 (51 51 (:TYPE-PRESCRIPTION RTL::INTEGERP-EA))
 (48 30 (:REWRITE DEFAULT-PLUS-1))
 (46 17 (:REWRITE EQUAL-OF-BOOLEANS-REWRITE))
 (44 11 (:REWRITE RATIONALP-X))
 (36 6 (:REWRITE |(+ y x)|))
 (22 22 (:TYPE-PRESCRIPTION BOOLEANP))
 (18 18 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (18 18 (:REWRITE NORMALIZE-ADDENDS))
 (18 6 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (18 6 (:REWRITE DEFAULT-LESS-THAN-2))
 (17 17 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (17 17 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-EQUAL))
 (17 17 (:REWRITE REDUCE-ADDITIVE-CONSTANT-EQUAL))
 (17 17 (:REWRITE EQUAL-OF-PREDICATES-REWRITE))
 (17 17 (:REWRITE |(equal c (/ x))|))
 (17 17 (:REWRITE |(equal c (- x))|))
 (17 17 (:REWRITE |(equal (/ x) c)|))
 (17 17 (:REWRITE |(equal (/ x) (/ y))|))
 (17 17 (:REWRITE |(equal (- x) c)|))
 (17 17 (:REWRITE |(equal (- x) (- y))|))
 (12 6 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (11 11 (:REWRITE REDUCE-RATIONALP-+))
 (11 11 (:REWRITE REDUCE-RATIONALP-*))
 (11 11 (:REWRITE REDUCE-INTEGERP-+))
 (11 11 (:REWRITE RATIONALP-MINUS-X))
 (11 11 (:REWRITE INTEGERP-MINUS-X))
 (11 11 (:META META-RATIONALP-CORRECT))
 (11 11 (:META META-INTEGERP-CORRECT))
 (10 10 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-=-0))
 (6 6 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-POSITIVE-BASE))
 (6 6 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-ODD-EXPONENT))
 (6 6 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-EVEN-EXPONENT))
 (6 6 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONNEGATIVE-BASE))
 (6 6 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-ODD-EXPONENT))
 (6 6 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-EVEN-EXPONENT))
 (6 6 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-B))
 (6 6 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-A))
 (6 6 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE))
 (6 6 (:REWRITE THE-FLOOR-BELOW))
 (6 6 (:REWRITE THE-FLOOR-ABOVE))
 (6 6 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-0-<-AX+BX-RATIONAL-REMAINDER))
 (6 6 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-0-<-AX+BX-RATIONAL-COMMON))
 (6 6 (:REWRITE SIMPLIFY-SUMS-<))
 (6 6 (:REWRITE REMOVE-STRICT-INEQUALITIES))
 (6 6 (:REWRITE REDUCE-RATIONAL-MULTIPLICATIVE-CONSTANT-<))
 (6 6 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-<))
 (6 6 (:REWRITE INTEGERP-<-CONSTANT))
 (6 6 (:REWRITE FOLD-CONSTS-IN-+))
 (6 6 (:REWRITE DEFAULT-LESS-THAN-1))
 (6 6 (:REWRITE CONSTANT-<-INTEGERP))
 (6 6 (:REWRITE |(< y (+ (- c) x))|))
 (6 6 (:REWRITE |(< x (+ c/d y))|))
 (6 6 (:REWRITE |(< c (/ x)) positive c --- present in goal|))
 (6 6 (:REWRITE |(< c (/ x)) positive c --- obj t or nil|))
 (6 6 (:REWRITE |(< c (/ x)) negative c --- present in goal|))
 (6 6 (:REWRITE |(< c (/ x)) negative c --- obj t or nil|))
 (6 6 (:REWRITE |(< c (- x))|))
 (6 6 (:REWRITE |(< 0 (/ x))|))
 (6 6 (:REWRITE |(< 0 (* x y))|))
 (6 6 (:REWRITE |(< (/ x) c) positive c --- present in goal|))
 (6 6 (:REWRITE |(< (/ x) c) positive c --- obj t or nil|))
 (6 6 (:REWRITE |(< (/ x) c) negative c --- present in goal|))
 (6 6 (:REWRITE |(< (/ x) c) negative c --- obj t or nil|))
 (6 6 (:REWRITE |(< (/ x) (/ y))|))
 (6 6 (:REWRITE |(< (- x) c)|))
 (6 6 (:REWRITE |(< (- x) (- y))|))
 (6 6 (:REWRITE |(+ c (+ d x))|))
 (3 3 (:REWRITE REMOVE-WEAK-INEQUALITIES))
 )
(RTL::NOT-SPECIALP-FUSED
 (374509 60501 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (285534 31726 (:REWRITE ACL2-NUMBERP-X))
 (240826 3373 (:REWRITE RTL::CASE-1-10))
 (240755 3373 (:REWRITE RTL::CASE-2-REWRITE))
 (151135 8199 (:REWRITE RTL::BITS-TAIL-GEN))
 (149987 149987 (:TYPE-PRESCRIPTION NOT-INTEGERP-3A))
 (149987 149987 (:TYPE-PRESCRIPTION NOT-INTEGERP-2A))
 (135246 60501 (:REWRITE EQUAL-OF-BOOLEANS-REWRITE))
 (126904 31726 (:REWRITE RATIONALP-X))
 (84072 60501 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (82872 82872 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-EVEN-EXPONENT))
 (82872 82872 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-ODD-EXPONENT))
 (82872 82872 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-EVEN-EXPONENT))
 (82872 82872 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-B))
 (73750 73750 (:REWRITE SUBSETP-MEMBER . 4))
 (73750 73750 (:REWRITE SUBSETP-MEMBER . 3))
 (73750 73750 (:REWRITE SUBSETP-MEMBER . 2))
 (73750 73750 (:REWRITE SUBSETP-MEMBER . 1))
 (73750 73750 (:REWRITE INTERSECTP-MEMBER . 3))
 (73750 73750 (:REWRITE INTERSECTP-MEMBER . 2))
 (71918 15633 (:REWRITE DEFAULT-TIMES-2))
 (69323 28345 (:REWRITE DEFAULT-PLUS-2))
 (64856 340 (:LINEAR RTL::BVECP-BMUX4SIGNED))
 (63452 63452 (:TYPE-PRESCRIPTION BOOLEANP))
 (60660 60536 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-EQUAL))
 (60571 60536 (:REWRITE |(equal (/ x) (/ y))|))
 (60536 60536 (:REWRITE REDUCE-ADDITIVE-CONSTANT-EQUAL))
 (60536 60536 (:REWRITE |(equal c (/ x))|))
 (60536 60536 (:REWRITE |(equal (- x) (- y))|))
 (60501 60501 (:REWRITE EQUAL-OF-PREDICATES-REWRITE))
 (60501 60501 (:REWRITE |(equal c (- x))|))
 (60501 60501 (:REWRITE |(equal (- x) c)|))
 (52394 7538 (:REWRITE RTL::FINAL-7))
 (50873 28345 (:REWRITE DEFAULT-PLUS-1))
 (48512 48512 (:TYPE-PRESCRIPTION RTL::INTEGERP-EB))
 (47000 6420 (:REWRITE |(< y (+ (- c) x))|))
 (45346 15633 (:REWRITE DEFAULT-TIMES-1))
 (41681 15856 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (37862 37862 (:REWRITE REDUCE-INTEGERP-+))
 (37862 37862 (:REWRITE INTEGERP-MINUS-X))
 (37862 37862 (:META META-INTEGERP-CORRECT))
 (36338 18562 (:REWRITE DEFAULT-LESS-THAN-2))
 (36110 36110 (:TYPE-PRESCRIPTION RTL::INTEGERP-EA))
 (35267 35267 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-=-0))
 (31726 31726 (:REWRITE REDUCE-RATIONALP-+))
 (31726 31726 (:REWRITE REDUCE-RATIONALP-*))
 (31726 31726 (:REWRITE RATIONALP-MINUS-X))
 (31726 31726 (:META META-RATIONALP-CORRECT))
 (28276 18562 (:REWRITE DEFAULT-LESS-THAN-1))
 (26266 26266 (:TYPE-PRESCRIPTION RTL::EXP11))
 (22349 1889 (:REWRITE |(< (+ (- c) x) y)|))
 (21113 8199 (:REWRITE RTL::BITS-TAIL))
 (18693 18539 (:REWRITE REDUCE-RATIONAL-MULTIPLICATIVE-CONSTANT-<))
 (18693 18539 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-<))
 (18562 18562 (:REWRITE THE-FLOOR-BELOW))
 (18562 18562 (:REWRITE THE-FLOOR-ABOVE))
 (17909 17909 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (16499 16499 (:REWRITE INTEGERP-<-CONSTANT))
 (16499 16499 (:REWRITE CONSTANT-<-INTEGERP))
 (16499 16499 (:REWRITE |(< c (/ x)) positive c --- present in goal|))
 (16499 16499 (:REWRITE |(< c (/ x)) positive c --- obj t or nil|))
 (16499 16499 (:REWRITE |(< c (/ x)) negative c --- present in goal|))
 (16499 16499 (:REWRITE |(< c (/ x)) negative c --- obj t or nil|))
 (16499 16499 (:REWRITE |(< c (- x))|))
 (16499 16499 (:REWRITE |(< (/ x) c) positive c --- present in goal|))
 (16499 16499 (:REWRITE |(< (/ x) c) positive c --- obj t or nil|))
 (16499 16499 (:REWRITE |(< (/ x) c) negative c --- present in goal|))
 (16499 16499 (:REWRITE |(< (/ x) c) negative c --- obj t or nil|))
 (16499 16499 (:REWRITE |(< (/ x) (/ y))|))
 (16499 16499 (:REWRITE |(< (- x) c)|))
 (16499 16499 (:REWRITE |(< (- x) (- y))|))
 (14840 340 (:LINEAR EXPT->=-1-ONE))
 (14840 340 (:LINEAR EXPT-<=-1-TWO))
 (14500 340 (:LINEAR EXPT->-1-ONE))
 (14500 340 (:LINEAR EXPT-<-1-TWO))
 (14160 340 (:LINEAR EXPT-X->=-X))
 (14160 340 (:LINEAR EXPT-X->-X))
 (12859 12859 (:REWRITE RTL::BITS-NEG-INDICES))
 (9241 9193 (:REWRITE REMOVE-WEAK-INEQUALITIES))
 (7455 7455 (:REWRITE |(< 0 (* x y))|))
 (7127 1501 (:REWRITE RTL::NEG-BITN-0))
 (6776 6775 (:REWRITE |(< 0 (/ x))|))
 (6741 1501 (:REWRITE RTL::NEG-BITN-1))
 (6420 6420 (:REWRITE |(< x (+ c/d y))|))
 (6161 6161 (:REWRITE RTL::SPECIALP-MAIN))
 (6161 6161 (:REWRITE RTL::FMUL64-MAIN))
 (6133 6133 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-0-<-AX+BX-RATIONAL-REMAINDER))
 (6133 6133 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-0-<-AX+BX-RATIONAL-COMMON))
 (4553 1501 (:REWRITE RTL::BVECP-BITN-0))
 (4156 3040 (:REWRITE EXPT-WITH-VIOLATED-GUARDS))
 (4134 4134 (:REWRITE FOLD-CONSTS-IN-+))
 (2636 1520 (:REWRITE DEFAULT-EXPT-2))
 (2380 2380 (:TYPE-PRESCRIPTION RTL::SI-EXPPRODINT-1))
 (2172 1482 (:TYPE-PRESCRIPTION BUBBLE-DOWN))
 (2040 2040 (:TYPE-PRESCRIPTION RTL::INTEGERP-OPB))
 (2040 2040 (:TYPE-PRESCRIPTION RTL::INTEGERP-OPA))
 (1889 1889 (:REWRITE |(< (+ c/d x) y)|))
 (1520 1520 (:REWRITE DEFAULT-EXPT-1))
 (1520 1520 (:REWRITE |(expt 1/c n)|))
 (1520 1520 (:REWRITE |(expt (- x) n)|))
 (1476 1476 (:REWRITE RTL::BITN-NEG))
 (979 979 (:REWRITE |(< (* x y) 0)|))
 (857 857 (:REWRITE |(equal (+ (- c) x) y)|))
 (680 680 (:LINEAR EXPT-IS-WEAKLY-INCREASING-FOR-BASE->-1))
 (680 680 (:LINEAR EXPT-IS-WEAKLY-DECREASING-FOR-POS-BASE-<-1))
 (680 680 (:LINEAR EXPT-IS-INCREASING-FOR-BASE->-1))
 (680 680 (:LINEAR EXPT-IS-DECREASING-FOR-POS-BASE-<-1))
 (672 672 (:REWRITE BUBBLE-DOWN-*-MATCH-3))
 (672 672 (:REWRITE |(* c (* d x))|))
 (532 532 (:TYPE-PRESCRIPTION RTL::CAT-NONNEGATIVE-INTEGER-TYPE))
 (400 400 (:TYPE-PRESCRIPTION BINARY-LOGXOR))
 (340 340 (:REWRITE RTL::SI-EXPPRODINT-1))
 (340 340 (:REWRITE RTL::INTEGERP-OPB))
 (340 340 (:REWRITE RTL::INTEGERP-OPA))
 (340 340 (:REWRITE RTL::INTEGERP-EB))
 (340 340 (:REWRITE RTL::INTEGERP-EA))
 (340 340 (:LINEAR EXPT-LINEAR-UPPER-<=))
 (340 340 (:LINEAR EXPT-LINEAR-UPPER-<))
 (340 340 (:LINEAR EXPT-LINEAR-LOWER-<=))
 (340 340 (:LINEAR EXPT-LINEAR-LOWER-<))
 (340 340 (:LINEAR EXPT->=-1-TWO))
 (340 340 (:LINEAR EXPT->-1-TWO))
 (340 340 (:LINEAR EXPT-<=-1-ONE))
 (340 340 (:LINEAR EXPT-<-1-ONE))
 (299 299 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-<-0-RATIONAL-REMAINDER))
 (299 299 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-<-0-RATIONAL-COMMON))
 (299 299 (:REWRITE |(< (/ x) 0)|))
 (274 274 (:REWRITE |(< (* x y) 0) rationalp (* x y)|))
 (175 50 (:REWRITE RTL::BITN-BVECP-1))
 (150 50 (:REWRITE RTL::LOGXOR-BVECP))
 (105 35 (:REWRITE |(equal x (/ y))|))
 (72 72 (:TYPE-PRESCRIPTION RTL::SETBITS))
 (70 35 (:REWRITE DEFAULT-DIVIDE))
 (70 35 (:REWRITE |(not (equal x (/ y)))|))
 (36 36 (:TYPE-PRESCRIPTION RTL::LOGAND1))
 (36 1 (:REWRITE |(<= (/ x) y) with (< 0 x)|))
 (36 1 (:REWRITE |(< x (/ y)) with (< 0 y)|))
 (25 25 (:REWRITE DEFAULT-LOGXOR-2))
 (25 25 (:REWRITE DEFAULT-LOGXOR-1))
 (25 25 (:REWRITE RTL::BITN-BVECP))
 (3 1 (:REWRITE |(<= (/ x) y) with (< x 0)|))
 (3 1 (:REWRITE |(< x (/ y)) with (< y 0)|))
 (1 1 (:TYPE-PRESCRIPTION NOT-INTEGERP-3A-EXPT))
 (1 1 (:TYPE-PRESCRIPTION NOT-INTEGERP-2A-EXPT))
 (1 1 (:TYPE-PRESCRIPTION NOT-INTEGERP-1A-EXPT))
 )
(RTL::FMUL64-FUSED-MAIN)
(RTL::FMUL64-FUSED-LEMMA-TO-BE-FUNCTIONALLY-INSTANTIATED
 (408 34 (:REWRITE RTL::NEG-BITN-0))
 (204 24 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (195 35 (:REWRITE ACL2-NUMBERP-X))
 (102 34 (:REWRITE RTL::NEG-BITN-1))
 (82 34 (:REWRITE RTL::BVECP-BITN-0))
 (80 20 (:REWRITE RATIONALP-X))
 (69 5 (:REWRITE RTL::FMUL64-MAIN))
 (64 24 (:REWRITE EQUAL-OF-BOOLEANS-REWRITE))
 (47 5 (:REWRITE RTL::SPECIALP-MAIN))
 (42 42 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-ODD-EXPONENT))
 (42 42 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NONPOSITIVE-BASE-EVEN-EXPONENT))
 (42 42 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-ODD-EXPONENT))
 (42 42 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-NEGATIVE-BASE-EVEN-EXPONENT))
 (42 42 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-B))
 (42 42 (:TYPE-PRESCRIPTION EXPT-TYPE-PRESCRIPTION-INTEGERP-BASE-A))
 (40 40 (:TYPE-PRESCRIPTION BOOLEANP))
 (39 24 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (34 34 (:REWRITE RTL::BITN-NEG))
 (26 10 (:REWRITE RTL::FINAL-7))
 (24 24 (:REWRITE REDUCE-MULTIPLICATIVE-CONSTANT-EQUAL))
 (24 24 (:REWRITE REDUCE-ADDITIVE-CONSTANT-EQUAL))
 (24 24 (:REWRITE EQUAL-OF-PREDICATES-REWRITE))
 (24 24 (:REWRITE |(equal c (/ x))|))
 (24 24 (:REWRITE |(equal c (- x))|))
 (24 24 (:REWRITE |(equal (/ x) c)|))
 (24 24 (:REWRITE |(equal (/ x) (/ y))|))
 (24 24 (:REWRITE |(equal (- x) c)|))
 (24 24 (:REWRITE |(equal (- x) (- y))|))
 (20 20 (:REWRITE REDUCE-RATIONALP-+))
 (20 20 (:REWRITE REDUCE-RATIONALP-*))
 (20 20 (:REWRITE REDUCE-INTEGERP-+))
 (20 20 (:REWRITE RATIONALP-MINUS-X))
 (20 20 (:REWRITE INTEGERP-MINUS-X))
 (20 20 (:META META-RATIONALP-CORRECT))
 (20 20 (:META META-INTEGERP-CORRECT))
 (16 16 (:TYPE-PRESCRIPTION RTL::SPECIALP))
 (14 14 (:REWRITE RTL::BITS-REVERSE-INDICES))
 (14 14 (:REWRITE RTL::BITS-NEG-INDICES))
 (6 6 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-AX+BX-=-0))
 )
(RTL::FMUL64-FUSED-CORRECT
 (12 4 (:REWRITE RTL::BVECP-BITN-0))
 (10 4 (:REWRITE RTL::NEG-BITN-1))
 (10 4 (:REWRITE RTL::NEG-BITN-0))
 (4 4 (:REWRITE REDUCE-INTEGERP-+))
 (4 4 (:REWRITE INTEGERP-MINUS-X))
 (4 4 (:REWRITE RTL::BITN-NEG))
 (4 4 (:META META-INTEGERP-CORRECT))
 (2 2 (:REWRITE RTL::BITS-REVERSE-INDICES))
 (2 2 (:REWRITE RTL::BITS-NEG-INDICES))
 )
