(ABNF::ALISTP-WHEN-SYMBOL-ALISTP)
(ABNF::DEFTREEOPS-TABLE-LOOKUP)
(ABNF::BOOLEANP-OF-DEFTREEOPS-TABLE-LOOKUP)
(ABNF::DEFTREEOPS-TABLE-ADD)
(ABNF::PSEUDO-EVENT-FORMP-OF-DEFTREEOPS-TABLE-ADD)
(ABNF::DEFTREEOPS-PROCESS-GRAMMAR
 (78 4 (:REWRITE SYMBOLP-OF-CAR-WHEN-SYMBOL-LISTP))
 (55 5 (:DEFINITION ASSOC-EQUAL))
 (45 3 (:DEFINITION SYMBOL-LISTP))
 (40 25 (:REWRITE DEFAULT-CAR))
 (38 4 (:REWRITE SYMBOL-LISTP-OF-CDR-WHEN-SYMBOL-LISTP))
 (32 20 (:REWRITE DEFAULT-CDR))
 (32 2 (:REWRITE SYMBOLP-OF-CAAR-WHEN-SYMBOL-SYMBOL-ALISTP))
 (25 25 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (25 25 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (25 25 (:REWRITE CONSP-BY-LEN))
 (25 5 (:DEFINITION NTH))
 (25 1 (:REWRITE SYMBOL-SYMBOL-ALISTP-OF-CDR-WHEN-SYMBOL-SYMBOL-ALISTP))
 (15 6 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (12 4 (:REWRITE STATE-P-IMPLIES-AND-FORWARD-TO-STATE-P1))
 (8 8 (:REWRITE SYMBOLP-OF-CAR-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP))
 (5 2 (:REWRITE SYMBOL-SYMBOL-ALISTP-WHEN-NOT-CONSP))
 (4 4 (:REWRITE SYMBOL-SYMBOL-ALISTP-WHEN-SUBSETP-EQUAL))
 (4 4 (:REWRITE CONSP-OF-CDR-BY-LEN))
 (1 1 (:REWRITE CONSP-OF-CDDR-BY-LEN))
 )
(ABNF::SYMBOLP-OF-DEFTREEOPS-PROCESS-GRAMMAR.GRAMMAR
 (11 1 (:DEFINITION ASSOC-EQUAL))
 (6 3 (:REWRITE DEFAULT-CDR))
 (6 3 (:REWRITE DEFAULT-CAR))
 (5 1 (:DEFINITION NTH))
 (4 4 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (4 4 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (4 4 (:REWRITE CONSP-BY-LEN))
 )
(ABNF::DEFTREEOPS-PROCESS-PREFIX)
(ABNF::SYMBOLP-OF-DEFTREEOPS-PROCESS-PREFIX.PREFIX)
(ABNF::DEFTREEOPS-PROCESS-INPUTS
 (24 2 (:REWRITE SET::SETS-ARE-TRUE-LISTS-CHEAP))
 (18 1 (:DEFINITION TRUE-LISTP))
 (12 12 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (12 12 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (12 12 (:REWRITE CONSP-BY-LEN))
 (11 1 (:DEFINITION ASSOC-EQUAL))
 (7 4 (:REWRITE DEFAULT-CAR))
 (6 6 (:REWRITE DEFAULT-CDR))
 (6 2 (:REWRITE STATE-P-IMPLIES-AND-FORWARD-TO-STATE-P1))
 (5 5 (:REWRITE CONSP-OF-CDR-BY-LEN))
 (4 4 (:TYPE-PRESCRIPTION SET::SETP-TYPE))
 (4 2 (:REWRITE ABNF::SETP-WHEN-TREE-SETP))
 (4 2 (:REWRITE ABNF::SETP-WHEN-RULENAME-SETP))
 (4 2 (:REWRITE OMAP::SETP-WHEN-MAPP))
 (4 2 (:REWRITE SET::NONEMPTY-MEANS-SET))
 (2 2 (:TYPE-PRESCRIPTION ABNF::TREE-SETP))
 (2 2 (:TYPE-PRESCRIPTION ABNF::RULENAME-SETP))
 (2 2 (:TYPE-PRESCRIPTION OMAP::MAPP))
 (2 2 (:TYPE-PRESCRIPTION SET::EMPTY-TYPE))
 (2 2 (:REWRITE TRUE-LISTP-WHEN-UNSIGNED-BYTE-LISTP))
 (2 2 (:REWRITE SET::IN-SET))
 )
(ABNF::RETURN-TYPE-OF-DEFTREEOPS-PROCESS-INPUTS.VAL
 (236 118 (:TYPE-PRESCRIPTION TRUE-LISTP-OF-PARTITION-REST-AND-KEYWORD.REST))
 (118 118 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (110 10 (:DEFINITION ASSOC-EQUAL))
 (77 47 (:REWRITE DEFAULT-CAR))
 (37 37 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (37 37 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (37 37 (:REWRITE CONSP-BY-LEN))
 (33 24 (:REWRITE DEFAULT-CDR))
 (6 6 (:REWRITE CONSP-OF-CDR-BY-LEN))
 )
(ABNF::TRUE-LISTP-OF-DEFTREEOPS-PROCESS-INPUTS.VAL
 (24 2 (:REWRITE SET::SETS-ARE-TRUE-LISTS-CHEAP))
 (18 1 (:DEFINITION TRUE-LISTP))
 (11 1 (:DEFINITION ASSOC-EQUAL))
 (7 4 (:REWRITE DEFAULT-CAR))
 (6 6 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (6 6 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (6 6 (:REWRITE CONSP-BY-LEN))
 (6 3 (:TYPE-PRESCRIPTION TRUE-LISTP-OF-PARTITION-REST-AND-KEYWORD.REST))
 (4 4 (:TYPE-PRESCRIPTION SET::SETP-TYPE))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 2 (:REWRITE ABNF::SETP-WHEN-TREE-SETP))
 (4 2 (:REWRITE ABNF::SETP-WHEN-RULENAME-SETP))
 (4 2 (:REWRITE OMAP::SETP-WHEN-MAPP))
 (4 2 (:REWRITE SET::NONEMPTY-MEANS-SET))
 (2 2 (:TYPE-PRESCRIPTION ABNF::TREE-SETP))
 (2 2 (:TYPE-PRESCRIPTION ABNF::RULENAME-SETP))
 (2 2 (:TYPE-PRESCRIPTION OMAP::MAPP))
 (2 2 (:TYPE-PRESCRIPTION SET::EMPTY-TYPE))
 (2 2 (:REWRITE TRUE-LISTP-WHEN-UNSIGNED-BYTE-LISTP))
 (2 2 (:REWRITE SET::IN-SET))
 (1 1 (:REWRITE CONSP-OF-CDR-BY-LEN))
 )
(ABNF::DEFTREEOPS-GEN-CST-MATCH)
(ABNF::PSEUDO-EVENT-FORM-LISTP-OF-DEFTREEOPS-GEN-CST-MATCH
 (1252 42 (:DEFINITION BINARY-APPEND))
 (1193 3 (:DEFINITION PSEUDO-EVENT-FORM-LISTP))
 (845 3 (:REWRITE PSEUDO-EVENT-FORM-LISTP-OF-CDR-WHEN-PSEUDO-EVENT-FORM-LISTP))
 (588 84 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (321 45 (:REWRITE DEFAULT-CDR))
 (321 45 (:REWRITE DEFAULT-CAR))
 (228 156 (:REWRITE STR::CONSP-OF-EXPLODE))
 (201 201 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (201 201 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (201 201 (:REWRITE CONSP-BY-LEN))
 (192 40 (:REWRITE INTERN-IN-PACKAGE-OF-SYMBOL-IS-IDENTITY))
 (156 24 (:REWRITE PSEUDO-EVENT-FORM-LISTP-WHEN-NOT-CONSP))
 (141 3 (:REWRITE PSEUDO-EVENT-FORMP-OF-CAR-WHEN-PSEUDO-EVENT-FORM-LISTP))
 (102 40 (:REWRITE DEFAULT-INTERN-IN-PACKAGE-OF-SYMBOL))
 (68 68 (:REWRITE APPEND-ATOM-UNDER-LIST-EQUIV))
 (66 40 (:REWRITE DEFAULT-SYMBOL-NAME))
 (62 24 (:REWRITE DEFAULT-SYMBOL-PACKAGE-NAME))
 (60 60 (:TYPE-PRESCRIPTION MEMBER-SYMBOL-NAME))
 (48 48 (:REWRITE PSEUDO-EVENT-FORM-LISTP-WHEN-SUBSETP-EQUAL))
 (45 45 (:REWRITE CONSP-OF-CDR-BY-LEN))
 (42 14 (:REWRITE SYMBOL-PACKAGE-NAME-INTERN-IN-PACKAGE-OF-SYMBOL))
 (38 38 (:REWRITE STR::EXPLODE-WHEN-NOT-STRINGP))
 (14 7 (:REWRITE DEFAULT-PKG-IMPORTS))
 (6 6 (:REWRITE PSEUDO-EVENT-FORMP-WHEN-MEMBER-EQUAL-OF-PSEUDO-EVENT-FORM-LISTP))
 (3 3 (:TYPE-PRESCRIPTION PSEUDO-EVENT-FORMP))
 )
(ABNF::DEFTREEOPS-GEN-CST-LIST-ELEM-MATCH)
(ABNF::PSEUDO-EVENT-FORM-LISTP-OF-DEFTREEOPS-GEN-CST-LIST-ELEM-MATCH
 (1252 42 (:DEFINITION BINARY-APPEND))
 (1193 3 (:DEFINITION PSEUDO-EVENT-FORM-LISTP))
 (845 3 (:REWRITE PSEUDO-EVENT-FORM-LISTP-OF-CDR-WHEN-PSEUDO-EVENT-FORM-LISTP))
 (588 84 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (321 45 (:REWRITE DEFAULT-CDR))
 (321 45 (:REWRITE DEFAULT-CAR))
 (228 156 (:REWRITE STR::CONSP-OF-EXPLODE))
 (201 201 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (201 201 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (201 201 (:REWRITE CONSP-BY-LEN))
 (192 40 (:REWRITE INTERN-IN-PACKAGE-OF-SYMBOL-IS-IDENTITY))
 (156 24 (:REWRITE PSEUDO-EVENT-FORM-LISTP-WHEN-NOT-CONSP))
 (141 3 (:REWRITE PSEUDO-EVENT-FORMP-OF-CAR-WHEN-PSEUDO-EVENT-FORM-LISTP))
 (102 40 (:REWRITE DEFAULT-INTERN-IN-PACKAGE-OF-SYMBOL))
 (68 68 (:REWRITE APPEND-ATOM-UNDER-LIST-EQUIV))
 (66 40 (:REWRITE DEFAULT-SYMBOL-NAME))
 (62 24 (:REWRITE DEFAULT-SYMBOL-PACKAGE-NAME))
 (60 60 (:TYPE-PRESCRIPTION MEMBER-SYMBOL-NAME))
 (48 48 (:REWRITE PSEUDO-EVENT-FORM-LISTP-WHEN-SUBSETP-EQUAL))
 (45 45 (:REWRITE CONSP-OF-CDR-BY-LEN))
 (42 14 (:REWRITE SYMBOL-PACKAGE-NAME-INTERN-IN-PACKAGE-OF-SYMBOL))
 (38 38 (:REWRITE STR::EXPLODE-WHEN-NOT-STRINGP))
 (14 7 (:REWRITE DEFAULT-PKG-IMPORTS))
 (6 6 (:REWRITE PSEUDO-EVENT-FORMP-WHEN-MEMBER-EQUAL-OF-PSEUDO-EVENT-FORM-LISTP))
 (3 3 (:TYPE-PRESCRIPTION PSEUDO-EVENT-FORMP))
 )
(ABNF::DEFTREEOPS-GEN-CST-LIST-REP-MATCH)
(ABNF::PSEUDO-EVENT-FORM-LISTP-OF-DEFTREEOPS-GEN-CST-LIST-REP-MATCH
 (1252 42 (:DEFINITION BINARY-APPEND))
 (1193 3 (:DEFINITION PSEUDO-EVENT-FORM-LISTP))
 (845 3 (:REWRITE PSEUDO-EVENT-FORM-LISTP-OF-CDR-WHEN-PSEUDO-EVENT-FORM-LISTP))
 (588 84 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (321 45 (:REWRITE DEFAULT-CDR))
 (321 45 (:REWRITE DEFAULT-CAR))
 (228 156 (:REWRITE STR::CONSP-OF-EXPLODE))
 (201 201 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (201 201 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (201 201 (:REWRITE CONSP-BY-LEN))
 (192 40 (:REWRITE INTERN-IN-PACKAGE-OF-SYMBOL-IS-IDENTITY))
 (156 24 (:REWRITE PSEUDO-EVENT-FORM-LISTP-WHEN-NOT-CONSP))
 (141 3 (:REWRITE PSEUDO-EVENT-FORMP-OF-CAR-WHEN-PSEUDO-EVENT-FORM-LISTP))
 (102 40 (:REWRITE DEFAULT-INTERN-IN-PACKAGE-OF-SYMBOL))
 (68 68 (:REWRITE APPEND-ATOM-UNDER-LIST-EQUIV))
 (66 40 (:REWRITE DEFAULT-SYMBOL-NAME))
 (62 24 (:REWRITE DEFAULT-SYMBOL-PACKAGE-NAME))
 (60 60 (:TYPE-PRESCRIPTION MEMBER-SYMBOL-NAME))
 (48 48 (:REWRITE PSEUDO-EVENT-FORM-LISTP-WHEN-SUBSETP-EQUAL))
 (45 45 (:REWRITE CONSP-OF-CDR-BY-LEN))
 (42 14 (:REWRITE SYMBOL-PACKAGE-NAME-INTERN-IN-PACKAGE-OF-SYMBOL))
 (38 38 (:REWRITE STR::EXPLODE-WHEN-NOT-STRINGP))
 (14 7 (:REWRITE DEFAULT-PKG-IMPORTS))
 (6 6 (:REWRITE PSEUDO-EVENT-FORMP-WHEN-MEMBER-EQUAL-OF-PSEUDO-EVENT-FORM-LISTP))
 (3 3 (:TYPE-PRESCRIPTION PSEUDO-EVENT-FORMP))
 )
(ABNF::DEFTREEOPS-GEN-CST-LIST-LIST-CONC-MATCH)
(ABNF::PSEUDO-EVENT-FORM-LISTP-OF-DEFTREEOPS-GEN-CST-LIST-LIST-CONC-MATCH
 (1252 42 (:DEFINITION BINARY-APPEND))
 (1193 3 (:DEFINITION PSEUDO-EVENT-FORM-LISTP))
 (845 3 (:REWRITE PSEUDO-EVENT-FORM-LISTP-OF-CDR-WHEN-PSEUDO-EVENT-FORM-LISTP))
 (588 84 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (321 45 (:REWRITE DEFAULT-CDR))
 (321 45 (:REWRITE DEFAULT-CAR))
 (228 156 (:REWRITE STR::CONSP-OF-EXPLODE))
 (201 201 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (201 201 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (201 201 (:REWRITE CONSP-BY-LEN))
 (192 40 (:REWRITE INTERN-IN-PACKAGE-OF-SYMBOL-IS-IDENTITY))
 (156 24 (:REWRITE PSEUDO-EVENT-FORM-LISTP-WHEN-NOT-CONSP))
 (141 3 (:REWRITE PSEUDO-EVENT-FORMP-OF-CAR-WHEN-PSEUDO-EVENT-FORM-LISTP))
 (102 40 (:REWRITE DEFAULT-INTERN-IN-PACKAGE-OF-SYMBOL))
 (68 68 (:REWRITE APPEND-ATOM-UNDER-LIST-EQUIV))
 (66 40 (:REWRITE DEFAULT-SYMBOL-NAME))
 (62 24 (:REWRITE DEFAULT-SYMBOL-PACKAGE-NAME))
 (60 60 (:TYPE-PRESCRIPTION MEMBER-SYMBOL-NAME))
 (48 48 (:REWRITE PSEUDO-EVENT-FORM-LISTP-WHEN-SUBSETP-EQUAL))
 (45 45 (:REWRITE CONSP-OF-CDR-BY-LEN))
 (42 14 (:REWRITE SYMBOL-PACKAGE-NAME-INTERN-IN-PACKAGE-OF-SYMBOL))
 (38 38 (:REWRITE STR::EXPLODE-WHEN-NOT-STRINGP))
 (14 7 (:REWRITE DEFAULT-PKG-IMPORTS))
 (6 6 (:REWRITE PSEUDO-EVENT-FORMP-WHEN-MEMBER-EQUAL-OF-PSEUDO-EVENT-FORM-LISTP))
 (3 3 (:TYPE-PRESCRIPTION PSEUDO-EVENT-FORMP))
 )
(ABNF::DEFTREEOPS-GEN-CST-LIST-LIST-ALT-MATCH)
(ABNF::PSEUDO-EVENT-FORM-LISTP-OF-DEFTREEOPS-GEN-CST-LIST-LIST-ALT-MATCH
 (1252 42 (:DEFINITION BINARY-APPEND))
 (1193 3 (:DEFINITION PSEUDO-EVENT-FORM-LISTP))
 (845 3 (:REWRITE PSEUDO-EVENT-FORM-LISTP-OF-CDR-WHEN-PSEUDO-EVENT-FORM-LISTP))
 (588 84 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (321 45 (:REWRITE DEFAULT-CDR))
 (321 45 (:REWRITE DEFAULT-CAR))
 (228 156 (:REWRITE STR::CONSP-OF-EXPLODE))
 (201 201 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (201 201 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (201 201 (:REWRITE CONSP-BY-LEN))
 (192 40 (:REWRITE INTERN-IN-PACKAGE-OF-SYMBOL-IS-IDENTITY))
 (156 24 (:REWRITE PSEUDO-EVENT-FORM-LISTP-WHEN-NOT-CONSP))
 (141 3 (:REWRITE PSEUDO-EVENT-FORMP-OF-CAR-WHEN-PSEUDO-EVENT-FORM-LISTP))
 (102 40 (:REWRITE DEFAULT-INTERN-IN-PACKAGE-OF-SYMBOL))
 (68 68 (:REWRITE APPEND-ATOM-UNDER-LIST-EQUIV))
 (66 40 (:REWRITE DEFAULT-SYMBOL-NAME))
 (62 24 (:REWRITE DEFAULT-SYMBOL-PACKAGE-NAME))
 (60 60 (:TYPE-PRESCRIPTION MEMBER-SYMBOL-NAME))
 (48 48 (:REWRITE PSEUDO-EVENT-FORM-LISTP-WHEN-SUBSETP-EQUAL))
 (45 45 (:REWRITE CONSP-OF-CDR-BY-LEN))
 (42 14 (:REWRITE SYMBOL-PACKAGE-NAME-INTERN-IN-PACKAGE-OF-SYMBOL))
 (38 38 (:REWRITE STR::EXPLODE-WHEN-NOT-STRINGP))
 (14 7 (:REWRITE DEFAULT-PKG-IMPORTS))
 (6 6 (:REWRITE PSEUDO-EVENT-FORMP-WHEN-MEMBER-EQUAL-OF-PSEUDO-EVENT-FORM-LISTP))
 (3 3 (:TYPE-PRESCRIPTION PSEUDO-EVENT-FORMP))
 )
(ABNF::DEFTREEOPS-GEN-MATCHERS)
(ABNF::PSEUDO-EVENT-FORM-LISTP-OF-DEFTREEOPS-GEN-MATCHERS
 (56 4 (:DEFINITION BINARY-APPEND))
 (40 8 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (8 8 (:REWRITE PSEUDO-EVENT-FORM-LISTP-WHEN-SUBSETP-EQUAL))
 (8 4 (:REWRITE DEFAULT-CDR))
 (8 4 (:REWRITE DEFAULT-CAR))
 (4 4 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (4 4 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (4 4 (:REWRITE CONSP-OF-CDR-BY-LEN))
 (4 4 (:REWRITE CONSP-BY-LEN))
 )
(ABNF::DEFTREEOPS-GEN-EVERYTHING
 (3 3 (:REWRITE DEFAULT-SYMBOL-NAME))
 (2 2 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE DEFAULT-CAR))
 )
(ABNF::PSEUDO-EVENT-FORMP-OF-DEFTREEOPS-GEN-EVERYTHING
 (92 2 (:DEFINITION BINARY-APPEND))
 (58 12 (:REWRITE STR::CONSP-OF-EXPLODE))
 (38 4 (:REWRITE STR::EQUAL-OF-EMPTY-STRING-WITH-DOWNCASE-STRING))
 (27 4 (:REWRITE DEFAULT-CDR))
 (27 4 (:REWRITE DEFAULT-CAR))
 (26 4 (:DEFINITION ATOM))
 (14 14 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (14 14 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (14 14 (:REWRITE CONSP-BY-LEN))
 (9 9 (:TYPE-PRESCRIPTION STR::DOWNCASE-STRING))
 (9 7 (:REWRITE STR::EXPLODE-WHEN-NOT-STRINGP))
 (6 6 (:DEFINITION NOT))
 (3 3 (:TYPE-PRESCRIPTION STRINGP-OF-IMPLODE))
 (3 3 (:REWRITE DEFAULT-SYMBOL-NAME))
 (3 1 (:REWRITE DEFAULT-INTERN-IN-PACKAGE-OF-SYMBOL))
 (2 2 (:REWRITE CONSP-OF-CDR-BY-LEN))
 (2 1 (:REWRITE INTERN-IN-PACKAGE-OF-SYMBOL-IS-IDENTITY))
 (1 1 (:TYPE-PRESCRIPTION STRING-APPEND-LST))
 )
(ABNF::DEFTREEOPS-FN
 (78 4 (:REWRITE SYMBOLP-OF-CAR-WHEN-SYMBOL-LISTP))
 (66 6 (:DEFINITION ASSOC-EQUAL))
 (61 30 (:REWRITE DEFAULT-CAR))
 (45 3 (:DEFINITION SYMBOL-LISTP))
 (38 4 (:REWRITE SYMBOL-LISTP-OF-CDR-WHEN-SYMBOL-LISTP))
 (37 25 (:REWRITE DEFAULT-CDR))
 (33 33 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (33 33 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (33 33 (:REWRITE CONSP-BY-LEN))
 (32 2 (:REWRITE SYMBOLP-OF-CAAR-WHEN-SYMBOL-SYMBOL-ALISTP))
 (30 6 (:DEFINITION NTH))
 (25 1 (:REWRITE SYMBOL-SYMBOL-ALISTP-OF-CDR-WHEN-SYMBOL-SYMBOL-ALISTP))
 (24 2 (:REWRITE SET::SETS-ARE-TRUE-LISTS-CHEAP))
 (18 1 (:DEFINITION TRUE-LISTP))
 (15 6 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (8 8 (:REWRITE SYMBOLP-OF-CAR-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP))
 (6 6 (:REWRITE CONSP-OF-CDR-BY-LEN))
 (6 2 (:REWRITE STATE-P-IMPLIES-AND-FORWARD-TO-STATE-P1))
 (5 2 (:REWRITE SYMBOL-SYMBOL-ALISTP-WHEN-NOT-CONSP))
 (4 4 (:TYPE-PRESCRIPTION SET::SETP-TYPE))
 (4 4 (:REWRITE SYMBOL-SYMBOL-ALISTP-WHEN-SUBSETP-EQUAL))
 (4 4 (:REWRITE PSEUDO-EVENT-FORMP-WHEN-MEMBER-EQUAL-OF-PSEUDO-EVENT-FORM-LISTP))
 (4 2 (:REWRITE ABNF::SETP-WHEN-TREE-SETP))
 (4 2 (:REWRITE ABNF::SETP-WHEN-RULENAME-SETP))
 (4 2 (:REWRITE OMAP::SETP-WHEN-MAPP))
 (4 2 (:REWRITE SET::NONEMPTY-MEANS-SET))
 (2 2 (:TYPE-PRESCRIPTION ABNF::TREE-SETP))
 (2 2 (:TYPE-PRESCRIPTION ABNF::RULENAME-SETP))
 (2 2 (:TYPE-PRESCRIPTION OMAP::MAPP))
 (2 2 (:TYPE-PRESCRIPTION SET::EMPTY-TYPE))
 (2 2 (:REWRITE TRUE-LISTP-WHEN-UNSIGNED-BYTE-LISTP))
 (2 2 (:REWRITE SET::IN-SET))
 (1 1 (:REWRITE CONSP-OF-CDDR-BY-LEN))
 )
(ABNF::PSEUDO-EVENT-FORMP-OF-DEFTREEOPS-FN.EVENT
 (21 5 (:REWRITE DEFAULT-CAR))
 (13 4 (:REWRITE DEFAULT-CDR))
 (11 1 (:DEFINITION ASSOC-EQUAL))
 (9 9 (:TYPE-PRESCRIPTION ABNF::TRUE-LISTP-OF-DEFTREEOPS-PROCESS-INPUTS.VAL))
 (7 7 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (7 7 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (7 7 (:REWRITE CONSP-BY-LEN))
 (5 1 (:DEFINITION NTH))
 (2 2 (:REWRITE PSEUDO-EVENT-FORMP-WHEN-MEMBER-EQUAL-OF-PSEUDO-EVENT-FORM-LISTP))
 (1 1 (:REWRITE CONSP-OF-CDR-BY-LEN))
 )