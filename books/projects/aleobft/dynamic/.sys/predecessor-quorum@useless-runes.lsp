(ALEOBFT-DYNAMIC::VALIDATOR-PREDECESSOR-QUORUM-P
 (12 4 (:REWRITE SETP-WHEN-POS-SETP))
 (12 4 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-MESSAGE-SETP))
 (12 4 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-CERTIFICATE-SETP))
 (12 4 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS+POS-SETP))
 (8 8 (:TYPE-PRESCRIPTION POS-SETP))
 (8 8 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::MESSAGE-SETP))
 (8 8 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-SETP))
 (8 8 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SETP))
 (7 6 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (7 6 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (7 6 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (6 6 (:REWRITE |(equal (- x) (- y))|))
 (6 2 (:REWRITE SET::EMPTYP-SUBSET-2))
 (6 2 (:REWRITE SET::EMPTYP-SUBSET))
 (2 2 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (2 2 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (2 2 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (2 2 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (2 2 (:REWRITE NORMALIZE-ADDENDS))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-IN-CERTIFICATE-SETP-BINDS-FREE-X))
 (2 1 (:REWRITE SIMPLIFY-SUMS-<))
 (2 1 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (2 1 (:REWRITE PREFER-POSITIVE-ADDENDS-<))
 (1 1 (:REWRITE POSP-WHEN-IN-POS-SETP-BINDS-FREE-X))
 (1 1 (:REWRITE |(< (- x) (- y))|))
 )
(ALEOBFT-DYNAMIC::BOOLEANP-OF-VALIDATOR-PREDECESSOR-QUORUM-P)
(ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P)
(ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-NECC
 (4 4 (:DEFINITION MV-NTH))
 )
(ALEOBFT-DYNAMIC::BOOLEANP-OF-PREDECESSOR-QUORUM-P)
(ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P
 (54 18 (:REWRITE SET::IN-TAIL))
 (24 24 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (24 12 (:REWRITE SET::NEVER-IN-EMPTY))
 (20 4 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (18 6 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (16 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (9 2 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-CERTIFICATE-OPTIONP))
 (7 1 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP-WHEN-CERTIFICATEP))
 (6 2 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-MESSAGE-SETP))
 (6 2 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS+POS-SETP))
 (4 4 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::MESSAGE-SETP))
 (4 4 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (4 4 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SETP))
 (4 4 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (3 1 (:REWRITE SETP-WHEN-POS-SETP))
 (2 2 (:TYPE-PRESCRIPTION POS-SETP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 )
(ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-WHEN-INIT
 (13 4 (:REWRITE SET::IN-TAIL))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-PREDECESSOR-QUORUM-P-OF-CREATE-CERTIFICATE-NEXT-OLD)
(ALEOBFT-DYNAMIC::SIGNER-IN-COMMITTEE-WHEN-VALIDATOR-SIGNER-QUORUM-P
 (3 3 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 1 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (3 1 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (3 1 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (3 1 (:REWRITE SET::EMPTYP-SUBSET-2))
 (3 1 (:REWRITE SET::EMPTYP-SUBSET))
 (2 2 (:REWRITE SET::SUBSET-TRANSITIVE))
 (2 1 (:REWRITE SET::DOUBLE-CONTAINMENT))
 (1 1 (:REWRITE SET::PICK-A-POINT-SUBSET-STRATEGY))
 (1 1 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (1 1 (:REWRITE |(equal (- x) (- y))|))
 )
(ALEOBFT-DYNAMIC::SIGNER-IN-COMMITTEE-WHEN-SIGNER-QUORUM-P
 (24 6 (:REWRITE SET::IN-TAIL))
 (12 12 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (9 3 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (9 3 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::SIGNER-IN-COMMITTEE-AT-ROUND-WHEN-SIGNER-QUORUM-P
 (296 74 (:REWRITE SET::IN-TAIL))
 (130 130 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (111 37 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (111 37 (:REWRITE SET::NEVER-IN-EMPTY))
 (24 6 (:REWRITE POS-FIX-WHEN-POSP))
 (22 22 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (18 6 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (18 6 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (18 6 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (12 12 (:TYPE-PRESCRIPTION POSP))
 (6 6 (:REWRITE POSP-WHEN-IN-POS-SETP-BINDS-FREE-X))
 (6 6 (:REWRITE |(equal (- x) (- y))|))
 (3 1 (:REWRITE SET::UNION-EMPTYP-Y))
 (3 1 (:REWRITE SET::UNION-EMPTYP-X))
 )
(ALEOBFT-DYNAMIC::AUTHOR-IN-COMMITTEE-AT-ROUND-WHEN-SIGNER-QUORUM-P
 (34 9 (:REWRITE SET::IN-TAIL))
 (16 16 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (12 4 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (12 4 (:REWRITE SET::NEVER-IN-EMPTY))
 (5 1 (:REWRITE SET::INSERT-IDENTITY))
 (3 1 (:REWRITE SET::INSERT-WHEN-EMPTYP))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 )
(ALEOBFT-DYNAMIC::LEMMA
 (80 20 (:REWRITE SET::IN-TAIL))
 (32 12 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (30 10 (:REWRITE SET::NEVER-IN-EMPTY))
 (6 6 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (3 3 (:REWRITE SET::HEAD-WHEN-EMPTYP))
 (3 3 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (2 1 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (2 1 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (2 1 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (1 1 (:REWRITE |(equal (- x) (- y))|))
 )
(ALEOBFT-DYNAMIC::AUTHORS-IN-COMMITTEE-AT-ROUND-WHEN-SIGNER-QUORUM-P
 (88 16 (:REWRITE SET::IN-TAIL))
 (30 18 (:REWRITE SET::SUBSET-IN-2))
 (28 12 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (25 11 (:REWRITE SET::EMPTYP-SUBSET))
 (24 8 (:REWRITE SET::NEVER-IN-EMPTY))
 (21 11 (:REWRITE SET::EMPTYP-SUBSET-2))
 (9 9 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (8 8 (:REWRITE SET::SUBSET-MEMBERSHIP-TAIL))
 (5 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (4 4 (:REWRITE SET::IN-TAIL-OR-HEAD))
 (4 4 (:REWRITE SET::HEAD-WHEN-EMPTYP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (2 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-PREDECESSOR-QUORUM-P-OF-CREATE-CERTIFICATE-NEXT-NEW
 (175 15 (:REWRITE SET::IN-TAIL))
 (98 38 (:REWRITE SET::EMPTYP-SUBSET-2))
 (94 38 (:REWRITE SET::EMPTYP-SUBSET))
 (90 22 (:REWRITE SET::SUBSET-IN-2))
 (66 8 (:LINEAR SET::PROPER-SUBSET-CARDINALITY))
 (37 37 (:REWRITE SET::PICK-A-POINT-SUBSET-STRATEGY))
 (37 37 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (27 8 (:REWRITE SET::SUBSET-MEMBERSHIP-TAIL))
 (24 9 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (24 8 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (21 8 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (21 7 (:REWRITE SET::NEVER-IN-EMPTY))
 (17 17 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (16 8 (:REWRITE SET::DOUBLE-CONTAINMENT))
 (9 9 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (9 9 (:REWRITE NORMALIZE-ADDENDS))
 (9 9 (:REWRITE |(equal (- x) (- y))|))
 (6 3 (:REWRITE |(equal (+ c x) d)|))
 (1 1 (:REWRITE POSP-WHEN-IN-POS-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::LEMMA
 (316 82 (:REWRITE SET::IN-TAIL))
 (164 164 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (114 38 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (114 38 (:REWRITE SET::NEVER-IN-EMPTY))
 (63 14 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-CERTIFICATE-OPTIONP))
 (48 16 (:REWRITE ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-WHEN-INIT))
 (42 7 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP-WHEN-CERTIFICATEP))
 (32 32 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::SYSTEM-INITP))
 (30 6 (:REWRITE SET::INSERT-IDENTITY))
 (21 21 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP))
 (18 6 (:REWRITE SET::INSERT-WHEN-EMPTYP))
 (15 3 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (14 14 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-IN-CERTIFICATE-SETP-BINDS-FREE-X))
 (12 12 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (12 12 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (12 12 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (12 12 (:REWRITE |(equal (- x) (- y))|))
 (6 6 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (6 3 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (3 3 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-OF-CREATE-CERTIFICATE-NEXT)
(ALEOBFT-DYNAMIC::VALIDATOR-PREDECESSOR-QUORUM-P-OF-RECEIVE-CERTIFICATE-NEXT-OLD
 (8 2 (:REWRITE SET::IN-TAIL))
 (6 2 (:REWRITE SET::EMPTYP-SUBSET-2))
 (6 2 (:REWRITE SET::EMPTYP-SUBSET))
 (5 2 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (5 2 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (5 2 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (2 2 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (2 2 (:REWRITE NORMALIZE-ADDENDS))
 (2 2 (:REWRITE |(equal (- x) (- y))|))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-PREDECESSOR-QUORUM-P-OF-RECEIVE-CERTIFICATE-NEXT-NEW
 (48 12 (:REWRITE SET::IN-TAIL))
 (18 6 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (16 6 (:REWRITE SET::EMPTYP-SUBSET-2))
 (16 6 (:REWRITE SET::EMPTYP-SUBSET))
 (12 3 (:REWRITE ALEOBFT-DYNAMIC::MESSAGE-FIX-WHEN-MESSAGEP))
 (10 4 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (10 4 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (10 4 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (6 6 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::MESSAGEP))
 (6 6 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (6 2 (:REWRITE SET::NEVER-IN-EMPTY))
 (5 5 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (4 4 (:REWRITE |(equal (- x) (- y))|))
 (3 3 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (3 3 (:REWRITE NORMALIZE-ADDENDS))
 (3 3 (:REWRITE ALEOBFT-DYNAMIC::MESSAGEP-WHEN-IN-MESSAGE-SETP-BINDS-FREE-X))
 (2 2 (:LINEAR SET::SUBSET-CARDINALITY))
 (2 2 (:LINEAR SET::PROPER-SUBSET-CARDINALITY))
 (1 1 (:REWRITE SIMPLIFY-TERMS-SUCH-AS-A+AB-=-0))
 (1 1 (:REWRITE |(equal (- x) 0)|))
 )
(ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-OF-RECEIVE-CERTIFICATE-NEXT
 (316 82 (:REWRITE SET::IN-TAIL))
 (164 164 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (114 38 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (114 38 (:REWRITE SET::NEVER-IN-EMPTY))
 (48 16 (:REWRITE ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-WHEN-INIT))
 (32 32 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::SYSTEM-INITP))
 (30 6 (:REWRITE SET::INSERT-IDENTITY))
 (18 6 (:REWRITE SET::INSERT-WHEN-EMPTYP))
 (15 3 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (12 12 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (12 12 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (12 12 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (12 12 (:REWRITE |(equal (- x) (- y))|))
 (6 6 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (6 3 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (3 3 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-PREDECESSOR-QUORUM-P-OF-STORE-CERTIFICATE-NEXT
 (8 2 (:REWRITE SET::IN-TAIL))
 (6 2 (:REWRITE SET::EMPTYP-SUBSET-2))
 (6 2 (:REWRITE SET::EMPTYP-SUBSET))
 (5 2 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (5 2 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (5 2 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (2 2 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (2 2 (:REWRITE NORMALIZE-ADDENDS))
 (2 2 (:REWRITE |(equal (- x) (- y))|))
 )
(ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-OF-STORE-CERTIFICATE-NEXT
 (184 46 (:REWRITE SET::IN-TAIL))
 (92 92 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (69 23 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (69 23 (:REWRITE SET::NEVER-IN-EMPTY))
 (36 12 (:REWRITE ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-WHEN-INIT))
 (24 24 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::SYSTEM-INITP))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-PREDECESSOR-QUORUM-P-OF-ADVANCE-ROUND-NEXT
 (8 2 (:REWRITE SET::IN-TAIL))
 (6 2 (:REWRITE SET::EMPTYP-SUBSET-2))
 (6 2 (:REWRITE SET::EMPTYP-SUBSET))
 (5 2 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (5 2 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (5 2 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (2 2 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (2 2 (:REWRITE NORMALIZE-ADDENDS))
 (2 2 (:REWRITE |(equal (- x) (- y))|))
 )
(ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-OF-ADVANCE-ROUND-NEXT
 (184 46 (:REWRITE SET::IN-TAIL))
 (92 92 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (69 23 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (69 23 (:REWRITE SET::NEVER-IN-EMPTY))
 (36 12 (:REWRITE ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-WHEN-INIT))
 (24 24 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::SYSTEM-INITP))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-PREDECESSOR-QUORUM-P-OF-COMMIT-ANCHORS-NEXT
 (387 387 (:TYPE-PRESCRIPTION NOT-INTEGERP-3A))
 (387 387 (:TYPE-PRESCRIPTION NOT-INTEGERP-2A))
 (387 387 (:TYPE-PRESCRIPTION NOT-INTEGERP-1A))
 (242 44 (:LINEAR SET::PROPER-SUBSET-CARDINALITY))
 (232 58 (:REWRITE SET::IN-TAIL))
 (175 175 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (175 175 (:REWRITE NORMALIZE-ADDENDS))
 (125 25 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (117 39 (:REWRITE SET::EMPTYP-SUBSET-2))
 (117 39 (:REWRITE SET::EMPTYP-SUBSET))
 (87 29 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (78 34 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (78 34 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (78 34 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (76 42 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-<))
 (70 46 (:REWRITE REMOVE-WEAK-INEQUALITIES-TWO))
 (66 38 (:REWRITE SIMPLIFY-SUMS-<))
 (51 17 (:REWRITE SET::NEVER-IN-EMPTY))
 (50 50 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (50 25 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (49 41 (:REWRITE REMOVE-WEAK-INEQUALITIES-ONE))
 (42 42 (:REWRITE |(< (- x) (- y))|))
 (39 39 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (39 39 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (34 34 (:REWRITE |(equal (- x) (- y))|))
 (29 29 (:META META-INTEGERP-CORRECT))
 (28 28 (:REWRITE NORMALIZE-FACTORS-GATHER-EXPONENTS))
 (25 25 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (24 24 (:TYPE-PRESCRIPTION NOT-INTEGERP-4B))
 (24 24 (:TYPE-PRESCRIPTION NOT-INTEGERP-3B))
 (24 24 (:TYPE-PRESCRIPTION NOT-INTEGERP-2B))
 (24 24 (:TYPE-PRESCRIPTION NOT-INTEGERP-1B))
 (24 24 (:REWRITE-QUOTED-CONSTANT ALEOBFT-DYNAMIC::BLOCK-LIST-FIX-UNDER-BLOCK-LIST-EQUIV))
 (18 18 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (18 18 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (13 13 (:REWRITE REDUCE-INTEGERP-+))
 (13 13 (:REWRITE INTEGERP-MINUS-X))
 (13 13 (:REWRITE |(integerp (* c x))|))
 (12 6 (:REWRITE |(< (+ c x) d)|))
 (9 9 (:REWRITE POSP-WHEN-IN-POS-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-OF-COMMIT-ANCHORS-NEXT
 (184 46 (:REWRITE SET::IN-TAIL))
 (92 92 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (69 23 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (69 23 (:REWRITE SET::NEVER-IN-EMPTY))
 (36 12 (:REWRITE ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-WHEN-INIT))
 (24 24 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::SYSTEM-INITP))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-PREDECESSOR-QUORUM-P-OF-TIMER-EXPIRES-NEXT
 (8 2 (:REWRITE SET::IN-TAIL))
 (6 2 (:REWRITE SET::EMPTYP-SUBSET-2))
 (6 2 (:REWRITE SET::EMPTYP-SUBSET))
 (5 2 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (5 2 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (5 2 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (2 2 (:REWRITE NORMALIZE-TERMS-SUCH-AS-A/A+B-+-B/A+B))
 (2 2 (:REWRITE NORMALIZE-ADDENDS))
 (2 2 (:REWRITE |(equal (- x) (- y))|))
 )
(ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-OF-TIMER-EXPIRES-NEXT
 (184 46 (:REWRITE SET::IN-TAIL))
 (92 92 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (69 23 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (69 23 (:REWRITE SET::NEVER-IN-EMPTY))
 (36 12 (:REWRITE ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-WHEN-INIT))
 (24 24 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::SYSTEM-INITP))
 )
(ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-OF-EVENT-NEXT
 (21 7 (:REWRITE ALEOBFT-DYNAMIC::PREDECESSOR-QUORUM-P-WHEN-INIT))
 (14 14 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::SYSTEM-INITP))
 (10 5 (:REWRITE SIMPLIFY-SUMS-EQUAL))
 (10 5 (:REWRITE SIMPLIFY-PRODUCTS-GATHER-EXPONENTS-EQUAL))
 (10 5 (:REWRITE PREFER-POSITIVE-ADDENDS-EQUAL))
 (5 5 (:REWRITE |(equal (- x) (- y))|))
 )
