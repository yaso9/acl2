(ABNF::RULES-AMBIGUOUSP)
(ABNF::RULES-AMBIGUOUSP-SUFF)
(ABNF::BOOLEANP-OF-RULES-AMBIGUOUSP)
(ABNF::RULES-AMBIGUOUSP)
(ABNF::NUM-VAL-UNAMBIGUOUS
 (615 11 (:REWRITE NAT-LIST-FIX-WHEN-NAT-LISTP))
 (528 11 (:DEFINITION NAT-LISTP))
 (288 124 (:REWRITE CONSP-UNDER-IFF-WHEN-TRUE-LISTP))
 (283 11 (:DEFINITION NATP))
 (250 22 (:REWRITE INTEGERP-OF-CAR-WHEN-INTEGER-LISTP))
 (242 8 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (231 15 (:REWRITE DEFAULT-<-1))
 (230 8 (:REWRITE ACL2-NUMBERP-OF-CAR-WHEN-ACL2-NUMBER-LISTP))
 (226 16 (:REWRITE RATIONALP-OF-CAR-WHEN-RATIONAL-LISTP))
 (198 11 (:DEFINITION INTEGER-LISTP))
 (187 15 (:REWRITE DEFAULT-<-2))
 (184 4 (:DEFINITION ACL2-NUMBER-LISTP))
 (160 8 (:DEFINITION RATIONAL-LISTP))
 (151 55 (:REWRITE DEFAULT-CAR))
 (150 71 (:REWRITE DEFAULT-CDR))
 (131 22 (:REWRITE NAT-LISTP-WHEN-NOT-CONSP))
 (96 96 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (96 22 (:REWRITE INTEGER-LISTP-WHEN-NOT-CONSP))
 (88 88 (:REWRITE CONSP-BY-LEN))
 (58 16 (:REWRITE RATIONAL-LISTP-WHEN-NOT-CONSP))
 (57 57 (:TYPE-PRESCRIPTION NAT-LISTP))
 (57 57 (:TYPE-PRESCRIPTION INTEGER-LISTP))
 (56 56 (:TYPE-PRESCRIPTION RATIONAL-LISTP))
 (55 55 (:REWRITE CAR-WHEN-ALL-EQUALP))
 (54 54 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (54 54 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (48 4 (:REWRITE ABNF::TREE-FIX-WHEN-TREEP))
 (38 8 (:REWRITE ACL2-NUMBER-LISTP-WHEN-NOT-CONSP))
 (36 8 (:REWRITE NAT-LIST-FIX-UNDER-IFF))
 (36 4 (:REWRITE LEN-WHEN-ATOM))
 (33 33 (:REWRITE CONSP-OF-CDR-BY-LEN))
 (28 28 (:TYPE-PRESCRIPTION ACL2-NUMBER-LISTP))
 (22 11 (:REWRITE NATP-OF-CAR-WHEN-NAT-LISTP))
 (20 4 (:REWRITE ABNF::TREEP-WHEN-TREE-OPTIONP))
 (16 8 (:REWRITE RATIONAL-LISTP-OF-CDR-WHEN-RATIONAL-LISTP))
 (14 7 (:REWRITE NAT-LISTP-OF-CDR-WHEN-NAT-LISTP))
 (14 7 (:REWRITE INTEGER-LISTP-OF-CDR-WHEN-INTEGER-LISTP))
 (12 12 (:TYPE-PRESCRIPTION ABNF::TREEP))
 (8 8 (:TYPE-PRESCRIPTION ABNF::TREE-OPTIONP))
 (8 8 (:REWRITE ABNF::TREEP-WHEN-MEMBER-EQUAL-OF-TREE-LISTP))
 (8 4 (:REWRITE ABNF::TREE-OPTIONP-WHEN-TREEP))
 (8 4 (:REWRITE ACL2-NUMBER-LISTP-OF-CDR-WHEN-ACL2-NUMBER-LISTP))
 (4 4 (:REWRITE ABNF::TREEP-WHEN-PARSE-TREEP))
 (4 4 (:REWRITE ABNF::TREEP-WHEN-IN-TREE-SETP-BINDS-FREE-X))
 )
(ABNF::CHAR-VAL-UNAMBIGUOUS
 (627 11 (:REWRITE NAT-LIST-FIX-WHEN-NAT-LISTP))
 (528 11 (:DEFINITION NAT-LISTP))
 (275 11 (:DEFINITION NATP))
 (242 22 (:REWRITE INTEGERP-OF-CAR-WHEN-INTEGER-LISTP))
 (198 11 (:DEFINITION INTEGER-LISTP))
 (172 70 (:REWRITE CONSP-UNDER-IFF-WHEN-TRUE-LISTP))
 (143 22 (:REWRITE NAT-LISTP-WHEN-NOT-CONSP))
 (111 45 (:REWRITE DEFAULT-CAR))
 (100 45 (:REWRITE DEFAULT-CDR))
 (88 22 (:REWRITE INTEGER-LISTP-WHEN-NOT-CONSP))
 (77 77 (:TYPE-PRESCRIPTION NAT-LISTP))
 (77 77 (:TYPE-PRESCRIPTION INTEGER-LISTP))
 (66 66 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (66 66 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (66 66 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (66 66 (:REWRITE CONSP-BY-LEN))
 (48 4 (:REWRITE ABNF::TREE-FIX-WHEN-TREEP))
 (45 45 (:REWRITE CAR-WHEN-ALL-EQUALP))
 (33 33 (:REWRITE CONSP-OF-CDR-BY-LEN))
 (28 4 (:REWRITE STR::EXPLODE-UNDER-IFF))
 (26 2 (:REWRITE ABNF::NATS-MATCH-SENSITIVE-CHARS-P-WHEN-ATOM-CHARS))
 (26 2 (:REWRITE ABNF::NATS-MATCH-INSENSITIVE-CHARS-P-WHEN-ATOM-CHARS))
 (22 11 (:REWRITE NATP-OF-CAR-WHEN-NAT-LISTP))
 (22 11 (:REWRITE NAT-LISTP-OF-CDR-WHEN-NAT-LISTP))
 (22 11 (:REWRITE INTEGER-LISTP-OF-CDR-WHEN-INTEGER-LISTP))
 (20 4 (:REWRITE ABNF::TREEP-WHEN-TREE-OPTIONP))
 (16 16 (:TYPE-PRESCRIPTION STR::TRUE-LISTP-OF-EXPLODE))
 (12 12 (:TYPE-PRESCRIPTION ABNF::TREEP))
 (12 12 (:TYPE-PRESCRIPTION ABNF::CHAR-VAL-SENSITIVE->GET$INLINE))
 (12 12 (:TYPE-PRESCRIPTION ABNF::CHAR-VAL-INSENSITIVE->GET$INLINE))
 (11 11 (:REWRITE DEFAULT-<-2))
 (11 11 (:REWRITE DEFAULT-<-1))
 (8 8 (:TYPE-PRESCRIPTION ABNF::TREE-OPTIONP))
 (8 8 (:REWRITE ABNF::TREEP-WHEN-MEMBER-EQUAL-OF-TREE-LISTP))
 (8 4 (:REWRITE ABNF::TREE-OPTIONP-WHEN-TREEP))
 (8 4 (:REWRITE STR::EXPLODE-WHEN-NOT-STRINGP))
 (4 4 (:REWRITE ABNF::TREEP-WHEN-PARSE-TREEP))
 (4 4 (:REWRITE ABNF::TREEP-WHEN-IN-TREE-SETP-BINDS-FREE-X))
 )
(ABNF::PROSE-VAL-AMBIGUOUS)
(ABNF::ELEMENT-UNAMBIGUOUSP)
(ABNF::ELEMENT-UNAMBIGUOUSP-NECC)
(ABNF::BOOLEANP-OF-ELEMENT-UNAMBIGUOUSP)
(ABNF::ELEMENT-UNAMBIGUOUSP)
(ABNF::ELEMENT-AMBIGUOUSP-REWRITE)
(ABNF::ELEMENT-NUM-VAL-UNAMBIGUOUS
 (24 4 (:REWRITE ABNF::TREEP-WHEN-TREE-OPTIONP))
 (18 2 (:REWRITE ABNF::TREE-OPTIONP-WHEN-TREEP))
 (8 8 (:REWRITE ABNF::TREEP-WHEN-MEMBER-EQUAL-OF-TREE-LISTP))
 (6 6 (:TYPE-PRESCRIPTION ABNF::TREE-OPTIONP))
 (4 4 (:TYPE-PRESCRIPTION ABNF::TREEP))
 (4 4 (:REWRITE ABNF::TREEP-WHEN-PARSE-TREEP))
 (4 4 (:REWRITE ABNF::TREEP-WHEN-IN-TREE-SETP-BINDS-FREE-X))
 (4 4 (:REWRITE ABNF::TREE-MATCH-ELEMENT-P-WHEN-MEMBER-EQUAL-OF-TREE-LIST-MATCH-ELEMENT-P))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 4 (:REWRITE DEFAULT-CAR))
 (4 4 (:REWRITE CAR-WHEN-ALL-EQUALP))
 (1 1 (:REWRITE ABNF::CHAR-VAL-UNAMBIGUOUS))
 )
(ABNF::ELEMENT-CHAR-VAL-UNAMBIGUOUS
 (24 4 (:REWRITE ABNF::TREEP-WHEN-TREE-OPTIONP))
 (18 2 (:REWRITE ABNF::TREE-OPTIONP-WHEN-TREEP))
 (8 8 (:REWRITE ABNF::TREEP-WHEN-MEMBER-EQUAL-OF-TREE-LISTP))
 (6 6 (:TYPE-PRESCRIPTION ABNF::TREE-OPTIONP))
 (4 4 (:TYPE-PRESCRIPTION ABNF::TREEP))
 (4 4 (:REWRITE ABNF::TREEP-WHEN-PARSE-TREEP))
 (4 4 (:REWRITE ABNF::TREEP-WHEN-IN-TREE-SETP-BINDS-FREE-X))
 (4 4 (:REWRITE ABNF::TREE-MATCH-ELEMENT-P-WHEN-MEMBER-EQUAL-OF-TREE-LIST-MATCH-ELEMENT-P))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 4 (:REWRITE DEFAULT-CAR))
 (4 4 (:REWRITE CAR-WHEN-ALL-EQUALP))
 (1 1 (:REWRITE ABNF::ELEMENT-NUM-VAL-UNAMBIGUOUS))
 )
(ABNF::ELEMENT-PROSE-VAL-AMBIGUOUS
 (4 4 (:REWRITE ABNF::TREE-MATCH-ELEMENT-P-WHEN-MEMBER-EQUAL-OF-TREE-LIST-MATCH-ELEMENT-P))
 (2 2 (:REWRITE-QUOTED-CONSTANT ABNF::TREE-OPTION-FIX-UNDER-TREE-OPTION-EQUIV))
 (2 2 (:REWRITE-QUOTED-CONSTANT ABNF::TREE-FIX-UNDER-TREE-EQUIV))
 (1 1 (:REWRITE ABNF::ELEMENT-NUM-VAL-UNAMBIGUOUS))
 (1 1 (:REWRITE ABNF::ELEMENT-CHAR-VAL-UNAMBIGUOUS))
 )
(ABNF::REPETITION-UNAMBIGUOUSP)
(ABNF::REPETITION-UNAMBIGUOUSP-NECC)
(ABNF::BOOLEANP-OF-REPETITION-UNAMBIGUOUSP)
(ABNF::REPETITION-UNAMBIGUOUSP)
(ABNF::REPETITION-UNAMBIGUOUSP-REWRITE)
(ABNF::EMPTY-REPETITION-UMABIGUOUS
 (16 4 (:REWRITE ABNF::TREE-LISTP-WHEN-SUBSETP-EQUAL))
 (12 2 (:REWRITE ABNF::TREE-LISTP-WHEN-NOT-CONSP))
 (6 1 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (4 4 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (4 4 (:REWRITE-QUOTED-CONSTANT IFIX-UNDER-INT-EQUIV))
 (4 4 (:REWRITE ABNF::TREE-LISTP-WHEN-MEMBER-EQUAL-OF-TREE-LIST-LISTP))
 (4 4 (:REWRITE ABNF::TREE-LIST-MATCH-ELEMENT-P-WHEN-SUBSETP-EQUAL))
 (4 2 (:REWRITE LEN-WHEN-ATOM))
 (4 2 (:REWRITE DEFAULT-<-1))
 (3 3 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (3 3 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (3 3 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (3 3 (:REWRITE CONSP-BY-LEN))
 (2 2 (:TYPE-PRESCRIPTION SUBSETP-EQUAL))
 (2 2 (:REWRITE DEFAULT-<-2))
 (2 1 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (2 1 (:REWRITE DEFAULT-CDR))
 (2 1 (:REWRITE DEFAULT-CAR))
 (1 1 (:REWRITE SUBSETP-TRANS2))
 (1 1 (:REWRITE SUBSETP-TRANS))
 (1 1 (:REWRITE CAR-WHEN-ALL-EQUALP))
 )
(ABNF::CONCATENATION-UNAMBIGUOUSP)
(ABNF::CONCATENATION-UNAMBIGUOUSP-NECC)
(ABNF::BOOLEANP-OF-CONCATENATION-UNAMBIGUOUSP)
(ABNF::CONCATENATION-UNAMBIGUOUSP)
(ABNF::CONCATENATION-UNAMBIGUOUSP-REWRITE)
(ABNF::EMPTY-CONCATENATION-UNAMBIGUOUS
 (16 4 (:REWRITE ABNF::TREE-LIST-LISTP-WHEN-SUBSETP-EQUAL))
 (12 2 (:REWRITE ABNF::TREE-LIST-LISTP-WHEN-NOT-CONSP))
 (6 1 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (3 3 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (3 3 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (3 3 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (3 3 (:REWRITE CONSP-BY-LEN))
 (2 2 (:TYPE-PRESCRIPTION SUBSETP-EQUAL))
 (2 1 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (1 1 (:REWRITE SUBSETP-TRANS2))
 (1 1 (:REWRITE SUBSETP-TRANS))
 )
(ABNF::ALTERNATION-UNAMBIGUOUSP)
(ABNF::ALTERNATION-UNAMBIGUOUSP-NECC)
(ABNF::BOOLEANP-OF-ALTERNATION-UNAMBIGUOUSP)
(ABNF::ALTERNATION-UNAMBIGUOUSP)
(ABNF::ALTERNATION-UNAMBIGUOUSP-REWRITE)
(ABNF::ALTERNATION-UNAMBIGUOUSP-OF-NIL
 (16 4 (:REWRITE ABNF::TREE-LIST-LISTP-WHEN-SUBSETP-EQUAL))
 (12 2 (:REWRITE ABNF::TREE-LIST-LISTP-WHEN-NOT-CONSP))
 (7 4 (:REWRITE CONSP-UNDER-IFF-WHEN-TRUE-LISTP))
 (6 1 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (3 3 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (3 3 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (3 3 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (3 3 (:REWRITE CONSP-BY-LEN))
 (2 2 (:TYPE-PRESCRIPTION SUBSETP-EQUAL))
 (2 1 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (1 1 (:REWRITE SUBSETP-TRANS2))
 (1 1 (:REWRITE SUBSETP-TRANS))
 )
(ABNF::CONCATENATION-ALTERNATION-DISJOINTP)
(ABNF::CONCATENATION-ALTERNATION-DISJOINTP-NECC
 (4 4 (:DEFINITION MV-NTH))
 )
(ABNF::BOOLEANP-OF-CONCATENATION-ALTERNATION-DISJOINTP)
(ABNF::CONCATENATION-ALTERNATION-DISJOINTP)
(ABNF::CONCATENATION-ALTERNATION-DISJOINTP-OF-NIL
 (16 4 (:REWRITE ABNF::TREE-LIST-LISTP-WHEN-SUBSETP-EQUAL))
 (12 2 (:REWRITE ABNF::TREE-LIST-LISTP-WHEN-NOT-CONSP))
 (9 5 (:REWRITE CONSP-UNDER-IFF-WHEN-TRUE-LISTP))
 (6 1 (:REWRITE ABNF::TREE-LIST-LIST-MATCH-CONCATENATION-P-WHEN-ATOM-CONCATENATION))
 (6 1 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (4 4 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (4 4 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (4 4 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (4 4 (:REWRITE CONSP-BY-LEN))
 (2 2 (:TYPE-PRESCRIPTION SUBSETP-EQUAL))
 (2 1 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (1 1 (:REWRITE SUBSETP-TRANS2))
 (1 1 (:REWRITE SUBSETP-TRANS))
 )
(ABNF::ALTERNATION-UNAMBIGUOUSP-OF-CONS-WHEN-DISJOINTP
 (191 107 (:REWRITE CONSP-UNDER-IFF-WHEN-TRUE-LISTP))
 (165 30 (:REWRITE ABNF::TREE-LIST-LIST-MATCH-CONCATENATION-P-WHEN-ATOM-CONCATENATION))
 (130 20 (:REWRITE ABNF::TREE-LIST-LIST-MATCH-ALTERNATION-P-WHEN-ATOM-ALTERNATION))
 (84 84 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (84 84 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (84 84 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (84 84 (:REWRITE CONSP-BY-LEN))
 (84 14 (:REWRITE ABNF::TREE-LIST-LISTP-WHEN-NOT-CONSP))
 (54 9 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (32 18 (:REWRITE ABNF::TREE-LIST-LIST->STRING-WHEN-ATOM))
 (18 9 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (14 14 (:REWRITE DEFAULT-CDR))
 (14 14 (:REWRITE DEFAULT-CAR))
 (14 14 (:REWRITE CAR-WHEN-ALL-EQUALP))
 (13 13 (:REWRITE SUBSETP-TRANS2))
 (13 13 (:REWRITE SUBSETP-TRANS))
 (10 10 (:REWRITE CONSP-OF-CDR-BY-LEN))
 )
