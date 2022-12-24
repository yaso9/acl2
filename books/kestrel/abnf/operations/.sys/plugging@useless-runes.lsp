(ABNF::RULE-PROSEP
 (10 2 (:REWRITE ABNF::RULEP-WHEN-RULE-OPTIONP))
 (7 1 (:REWRITE ABNF::RULE-OPTIONP-WHEN-RULEP))
 (6 5 (:REWRITE DEFAULT-CAR))
 (4 4 (:REWRITE ABNF::RULEP-WHEN-MEMBER-EQUAL-OF-RULELISTP))
 (4 4 (:REWRITE ABNF::REPETITIONP-WHEN-MEMBER-EQUAL-OF-CONCATENATIONP))
 (4 4 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (4 4 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (4 4 (:REWRITE ABNF::CONCATENATIONP-WHEN-SUBSETP-EQUAL))
 (4 4 (:REWRITE ABNF::CONCATENATIONP-WHEN-MEMBER-EQUAL-OF-ALTERNATIONP))
 (4 3 (:REWRITE DEFAULT-CDR))
 (3 3 (:TYPE-PRESCRIPTION ABNF::RULE-OPTIONP))
 (2 2 (:REWRITE ABNF::CONCATENATIONP-WHEN-NOT-CONSP))
 )
(ABNF::BOOLEANP-OF-RULE-PROSEP)
(ABNF::REMOVE-PROSE-RULES
 (16 16 (:REWRITE SUBSETP-TRANS2))
 (16 16 (:REWRITE SUBSETP-TRANS))
 (13 5 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (7 7 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (7 7 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (7 5 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (4 2 (:REWRITE ABNF::RULELISTP-WHEN-NOT-CONSP))
 (2 2 (:REWRITE DEFAULT-CDR))
 (1 1 (:REWRITE DEFAULT-CAR))
 )
(ABNF::RULELISTP-OF-REMOVE-PROSE-RULES
 (531 64 (:REWRITE ABNF::RULEP-WHEN-MEMBER-EQUAL-OF-RULELISTP))
 (315 15 (:DEFINITION MEMBER-EQUAL))
 (279 30 (:REWRITE SUBSETP-CAR-MEMBER))
 (195 32 (:REWRITE ABNF::RULEP-OF-CAR-WHEN-RULELISTP))
 (160 49 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (159 42 (:REWRITE ABNF::RULELISTP-WHEN-NOT-CONSP))
 (98 98 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (98 98 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (89 49 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (77 77 (:TYPE-PRESCRIPTION MEMBER-EQUAL))
 (62 2 (:REWRITE SUBSETP-OF-CONS))
 (56 56 (:REWRITE SUBSETP-TRANS2))
 (56 56 (:REWRITE SUBSETP-TRANS))
 (46 46 (:REWRITE DEFAULT-CAR))
 (30 30 (:REWRITE SUBSETP-MEMBER . 2))
 (30 30 (:REWRITE SUBSETP-MEMBER . 1))
 (18 18 (:REWRITE DEFAULT-CDR))
 )
(ABNF::PLUG-RULES)
(ABNF::RULELISTP-OF-PLUG-RULES
 (10 1 (:DEFINITION BINARY-APPEND))
 (9 2 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTY-TYPE))
 (3 3 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (3 3 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (3 1 (:REWRITE SET::DIFFERENCE-EMPTY-Y))
 (3 1 (:REWRITE SET::DIFFERENCE-EMPTY-X))
 (2 2 (:REWRITE ABNF::RULELISTP-WHEN-SUBSETP-EQUAL))
 (1 1 (:REWRITE DEFAULT-CDR))
 (1 1 (:REWRITE DEFAULT-CAR))
 )