(PFCS::LIFT-RULE-OMAP-IN-TO-IN-OF-KEYS
 (4 2 (:TYPE-PRESCRIPTION OMAP::IN-WHEN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION OMAP::EMPTY))
 )
(PFCS::LIFT-RULE-OMAP-CONSP-OF-IN-IFF-IN
 (8 4 (:TYPE-PRESCRIPTION OMAP::IN-WHEN-EMPTY))
 (4 4 (:TYPE-PRESCRIPTION OMAP::EMPTY))
 )
(PFCS::LIFT-RULE-NATP-OF-MOD
 (2 2 (:TYPE-PRESCRIPTION NONNEG-OF-MOD-TYPE))
 )
(PFCS::LIFT-RULE-NFIX-WHEN-NATP)
(PFCS::LIFT-INFOP
 (216 12 (:REWRITE OMAP::ALISTP-WHEN-MAPP))
 (72 12 (:REWRITE ALISTP-WHEN-HONS-DUPLICITY-ALIST-P))
 (60 12 (:REWRITE OMAP::MFIX-IMPLIES-MAPP))
 (60 12 (:REWRITE OMAP::MAPP-WHEN-NOT-EMPTY))
 (60 12 (:REWRITE PFCS::MAPP-WHEN-ASSIGNMENTP))
 (56 56 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-ATOM-LISTP))
 (48 48 (:TYPE-PRESCRIPTION OMAP::MAPP))
 (40 10 (:REWRITE FTY::PROVE-EQUAL-OF-CONS-WHEN-FIRST-QUOTEP))
 (36 12 (:REWRITE HONS-DUPLICITY-ALIST-P-WHEN-NOT-CONSP))
 (24 24 (:TYPE-PRESCRIPTION OMAP::MFIX))
 (24 24 (:TYPE-PRESCRIPTION HONS-DUPLICITY-ALIST-P))
 (24 24 (:TYPE-PRESCRIPTION OMAP::EMPTY))
 (24 24 (:TYPE-PRESCRIPTION PFCS::ASSIGNMENTP))
 (24 24 (:REWRITE PFCS::ASSIGNMENTP-WHEN-MEMBER-EQUAL-OF-ASSIGNMENT-LISTP))
 (24 12 (:REWRITE OMAP::MFIX-WHEN-MAPP))
 (24 12 (:REWRITE OMAP::MAPP-NON-NIL-IMPLIES-NON-EMPTY))
 (12 6 (:REWRITE CONSP-OF-CAR-WHEN-ATOM-LISTP))
 (6 6 (:TYPE-PRESCRIPTION ATOM-LISTP))
 )
(PFCS::CONSP-WHEN-LIFT-INFOP)
(PFCS::LIFT-INFO-FIX$INLINE)
(PFCS::LIFT-INFOP-OF-LIFT-INFO-FIX
 (14 6 (:REWRITE LIST-FIX-WHEN-TRUE-LISTP))
 (12 4 (:REWRITE PFCS::DEFINITION-FIX-WHEN-DEFINITIONP))
 (8 8 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (8 8 (:TYPE-PRESCRIPTION PFCS::DEFINITIONP))
 )
(PFCS::LIFT-INFO-FIX-WHEN-LIFT-INFOP
 (5 2 (:REWRITE FTY::PROVE-EQUAL-OF-CONS-WHEN-FIRST-QUOTEP))
 (1 1 (:DEFINITION STRIP-CARS))
 (1 1 (:DEFINITION ALISTP))
 )
(PFCS::LIFT-INFO-FIX$INLINE
 (216 12 (:REWRITE OMAP::ALISTP-WHEN-MAPP))
 (64 12 (:REWRITE ALISTP-WHEN-HONS-DUPLICITY-ALIST-P))
 (62 62 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-ATOM-LISTP))
 (60 12 (:REWRITE OMAP::MFIX-IMPLIES-MAPP))
 (60 12 (:REWRITE OMAP::MAPP-WHEN-NOT-EMPTY))
 (60 12 (:REWRITE PFCS::MAPP-WHEN-ASSIGNMENTP))
 (52 52 (:TYPE-PRESCRIPTION OMAP::MAPP))
 (48 4 (:REWRITE SET::SETS-ARE-TRUE-LISTS-CHEAP))
 (32 8 (:REWRITE PFCS::DEFINITIONP-WHEN-DEFINITION-OPTIONP))
 (30 4 (:REWRITE TRUE-LISTP-WHEN-SYMBOL-LISTP-REWRITE-BACKCHAIN-1))
 (30 4 (:REWRITE TRUE-LISTP-WHEN-STRING-LISTP-REWRITE))
 (28 16 (:REWRITE FTY::PROVE-EQUAL-OF-CONS-WHEN-FIRST-QUOTEP))
 (28 10 (:REWRITE HONS-DUPLICITY-ALIST-P-WHEN-NOT-CONSP))
 (24 24 (:TYPE-PRESCRIPTION OMAP::MFIX))
 (24 24 (:TYPE-PRESCRIPTION OMAP::EMPTY))
 (24 24 (:TYPE-PRESCRIPTION PFCS::ASSIGNMENTP))
 (24 24 (:REWRITE PFCS::ASSIGNMENTP-WHEN-MEMBER-EQUAL-OF-ASSIGNMENT-LISTP))
 (24 12 (:REWRITE OMAP::MFIX-WHEN-MAPP))
 (24 12 (:REWRITE OMAP::MAPP-NON-NIL-IMPLIES-NON-EMPTY))
 (21 21 (:TYPE-PRESCRIPTION HONS-DUPLICITY-ALIST-P))
 (20 4 (:REWRITE PFCS::DEFINITION-OPTIONP-WHEN-DEFINITIONP))
 (14 7 (:REWRITE CONSP-OF-CAR-WHEN-ATOM-LISTP))
 (12 12 (:TYPE-PRESCRIPTION PFCS::DEFINITION-OPTIONP))
 (8 8 (:TYPE-PRESCRIPTION SET::SETP-TYPE))
 (8 8 (:REWRITE SYMBOL-LISTP-WHEN-SUBSETP-EQUAL))
 (8 8 (:REWRITE STRING-LISTP-WHEN-SUBSETP-EQUAL))
 (8 8 (:REWRITE FTY::EQUAL-OF-CONS-BY-EQUAL-OF-STRIP-CARS))
 (8 4 (:REWRITE SETP-WHEN-STRING-SETP))
 (8 4 (:REWRITE OMAP::SETP-WHEN-MAPP))
 (8 4 (:REWRITE PFCS::SETP-WHEN-CONSTREL-SETP))
 (8 4 (:REWRITE SET::NONEMPTY-MEANS-SET))
 (6 3 (:REWRITE SYMBOL-LISTP-OF-CDR-WHEN-SYMBOL-LISTP))
 (6 3 (:REWRITE STRING-LISTP-OF-CDR-WHEN-STRING-LISTP))
 (4 4 (:TYPE-PRESCRIPTION STRING-SETP))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTY-TYPE))
 (4 4 (:TYPE-PRESCRIPTION PFCS::CONSTREL-SETP))
 (4 4 (:REWRITE PFIELD::TRUE-LISTP-WHEN-FE-LISTP))
 (4 4 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (4 4 (:REWRITE STRING-LISTP-WHEN-NOT-CONSP))
 (4 4 (:REWRITE SET::IN-SET))
 (3 2 (:REWRITE HONS-DUPLICITY-ALIST-P-OF-CONS))
 )
(FTY::TMP-DEFFIXTYPE-IDEMPOTENT)
(PFCS::LIFT-INFO-EQUIV$INLINE)
(PFCS::LIFT-INFO-EQUIV-IS-AN-EQUIVALENCE)
(PFCS::LIFT-INFO-EQUIV-IMPLIES-EQUAL-LIFT-INFO-FIX-1)
(PFCS::LIFT-INFO-FIX-UNDER-LIFT-INFO-EQUIV)
(PFCS::EQUAL-OF-LIFT-INFO-FIX-1-FORWARD-TO-LIFT-INFO-EQUIV)
(PFCS::EQUAL-OF-LIFT-INFO-FIX-2-FORWARD-TO-LIFT-INFO-EQUIV)
(PFCS::LIFT-INFO-EQUIV-OF-LIFT-INFO-FIX-1-FORWARD)
(PFCS::LIFT-INFO-EQUIV-OF-LIFT-INFO-FIX-2-FORWARD)
(PFCS::LIFT-INFO->DEF$INLINE
 (4 1 (:REWRITE FTY::PROVE-EQUAL-OF-CONS-WHEN-FIRST-QUOTEP))
 (1 1 (:DEFINITION STRIP-CARS))
 )
(PFCS::DEFINITIONP-OF-LIFT-INFO->DEF)
(PFCS::LIFT-INFO->DEF$INLINE-OF-LIFT-INFO-FIX-X
 (9 3 (:REWRITE PFCS::LIFT-INFO-FIX-WHEN-LIFT-INFOP))
 (6 6 (:TYPE-PRESCRIPTION PFCS::LIFT-INFOP))
 (3 1 (:REWRITE LIST-FIX-WHEN-TRUE-LISTP))
 (2 2 (:TYPE-PRESCRIPTION TRUE-LISTP))
 )
(PFCS::LIFT-INFO->DEF$INLINE-LIFT-INFO-EQUIV-CONGRUENCE-ON-X)
(PFCS::LIFT-INFO->HYPS$INLINE
 (4 1 (:REWRITE FTY::PROVE-EQUAL-OF-CONS-WHEN-FIRST-QUOTEP))
 )
(PFCS::TRUE-LISTP-OF-LIFT-INFO->HYPS)
(PFCS::LIFT-INFO->HYPS$INLINE-OF-LIFT-INFO-FIX-X
 (10 10 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (9 3 (:REWRITE PFCS::LIFT-INFO-FIX-WHEN-LIFT-INFOP))
 (6 6 (:TYPE-PRESCRIPTION PFCS::LIFT-INFOP))
 (6 6 (:TYPE-PRESCRIPTION PFCS::LIFT-INFO-FIX$INLINE))
 (3 1 (:REWRITE PFCS::DEFINITION-FIX-WHEN-DEFINITIONP))
 (2 2 (:TYPE-PRESCRIPTION PFCS::DEFINITIONP))
 )
(PFCS::LIFT-INFO->HYPS$INLINE-LIFT-INFO-EQUIV-CONGRUENCE-ON-X)
(PFCS::LIFT-INFO)
(PFCS::LIFT-INFOP-OF-LIFT-INFO
 (8 4 (:REWRITE LIST-FIX-WHEN-TRUE-LISTP))
 (6 2 (:REWRITE PFCS::DEFINITION-FIX-WHEN-DEFINITIONP))
 (4 4 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (4 4 (:TYPE-PRESCRIPTION PFCS::DEFINITIONP))
 )
(PFCS::LIFT-INFO->DEF-OF-LIFT-INFO)
(PFCS::LIFT-INFO->HYPS-OF-LIFT-INFO
 (10 10 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (6 6 (:TYPE-PRESCRIPTION PFCS::LIFT-INFO))
 )
(PFCS::LIFT-INFO-OF-FIELDS
 (3 1 (:REWRITE PFCS::LIFT-INFO-FIX-WHEN-LIFT-INFOP))
 (2 2 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (2 2 (:TYPE-PRESCRIPTION PFCS::LIFT-INFOP))
 )
(PFCS::LIFT-INFO-FIX-WHEN-LIFT-INFO
 (3 1 (:REWRITE PFCS::LIFT-INFO-FIX-WHEN-LIFT-INFOP))
 (2 2 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (2 2 (:TYPE-PRESCRIPTION PFCS::LIFT-INFOP))
 )
(PFCS::EQUAL-OF-LIFT-INFO
 (1 1 (:REWRITE FTY::PROVE-EQUAL-OF-CONS-WHEN-FIRST-QUOTEP))
 (1 1 (:REWRITE FTY::EQUAL-OF-CONS-BY-EQUAL-OF-STRIP-CARS))
 )
(PFCS::LIFT-INFO-OF-DEFINITION-FIX-DEF
 (4 2 (:REWRITE LIST-FIX-WHEN-TRUE-LISTP))
 (2 2 (:TYPE-PRESCRIPTION TRUE-LISTP))
 )
(PFCS::LIFT-INFO-DEFINITION-EQUIV-CONGRUENCE-ON-DEF)
(PFCS::LIFT-INFO-OF-LIST-FIX-HYPS
 (4 2 (:REWRITE PFCS::DEFINITION-FIX-WHEN-DEFINITIONP))
 (2 2 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (2 2 (:TYPE-PRESCRIPTION PFCS::DEFINITIONP))
 )
(PFCS::LIFT-INFO-LIST-EQUIV-CONGRUENCE-ON-HYPS)
(PFCS::LIFT-INFO-FIX-REDEF)
(PFCS::LIFT-THM-DEF-HYPS-AUX
 (54 3 (:REWRITE OMAP::ALISTP-WHEN-MAPP))
 (32 16 (:TYPE-PRESCRIPTION STRINGP-OF-HEAD-WHEN-STRING-SETP-TYPE-PRESCRIPTION))
 (18 3 (:REWRITE ALISTP-WHEN-HONS-DUPLICITY-ALIST-P))
 (16 16 (:TYPE-PRESCRIPTION STRING-SETP))
 (15 3 (:REWRITE OMAP::MFIX-IMPLIES-MAPP))
 (15 3 (:REWRITE OMAP::MAPP-WHEN-NOT-EMPTY))
 (15 3 (:REWRITE PFCS::MAPP-WHEN-ASSIGNMENTP))
 (12 12 (:TYPE-PRESCRIPTION OMAP::MAPP))
 (9 3 (:REWRITE HONS-DUPLICITY-ALIST-P-WHEN-NOT-CONSP))
 (8 8 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-ATOM-LISTP))
 (6 6 (:TYPE-PRESCRIPTION OMAP::MFIX))
 (6 6 (:TYPE-PRESCRIPTION HONS-DUPLICITY-ALIST-P))
 (6 6 (:TYPE-PRESCRIPTION OMAP::EMPTY))
 (6 6 (:TYPE-PRESCRIPTION PFCS::ASSIGNMENTP))
 (6 6 (:REWRITE PFCS::ASSIGNMENTP-WHEN-MEMBER-EQUAL-OF-ASSIGNMENT-LISTP))
 (6 3 (:REWRITE OMAP::MFIX-WHEN-MAPP))
 (6 3 (:REWRITE OMAP::MAPP-NON-NIL-IMPLIES-NON-EMPTY))
 (5 1 (:DEFINITION PFCS::LIFT-THM-DEF-HYPS-AUX))
 (3 3 (:REWRITE SET::TAIL-WHEN-EMPTY))
 (2 2 (:REWRITE SET::IN-TAIL-OR-HEAD))
 (2 2 (:REWRITE SET::HEAD-WHEN-EMPTY))
 (1 1 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (1 1 (:REWRITE PFCS::CONSTRELP-WHEN-IN-CONSTREL-SETP-BINDS-FREE-X))
 )
(PFCS::TRUE-LISTP-OF-LIFT-THM-DEF-HYPS-AUX)
(PFCS::LIFT-THM-DEF-HYPS)
(PFCS::TRUE-LISTP-OF-LIFT-THM-DEF-HYPS)
(PFCS::LIFT-THM-FREE-INST)
(PFCS::DOUBLET-LISTP-OF-LIFT-THM-FREE-INST
 (98 7 (:REWRITE SET::SETS-ARE-TRUE-LISTS-CHEAP))
 (56 7 (:REWRITE TRUE-LISTP-WHEN-SYMBOL-LISTP-REWRITE-BACKCHAIN-1))
 (56 7 (:REWRITE TRUE-LISTP-WHEN-STRING-LISTP-REWRITE))
 (42 7 (:REWRITE TRUE-LISTP-OF-CAR-WHEN-TRUE-LIST-LISTP))
 (28 28 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-ATOM-LISTP))
 (21 7 (:REWRITE CONSP-OF-CAR-WHEN-ATOM-LISTP))
 (14 14 (:TYPE-PRESCRIPTION TRUE-LIST-LISTP))
 (14 14 (:TYPE-PRESCRIPTION SYMBOL-LISTP))
 (14 14 (:TYPE-PRESCRIPTION STRING-LISTP))
 (14 14 (:TYPE-PRESCRIPTION SET::SETP-TYPE))
 (14 14 (:TYPE-PRESCRIPTION LEN))
 (14 14 (:REWRITE SYMBOL-LISTP-WHEN-SUBSETP-EQUAL))
 (14 14 (:REWRITE STRING-LISTP-WHEN-SUBSETP-EQUAL))
 (14 7 (:REWRITE SETP-WHEN-STRING-SETP))
 (14 7 (:REWRITE OMAP::SETP-WHEN-MAPP))
 (14 7 (:REWRITE PFCS::SETP-WHEN-CONSTREL-SETP))
 (14 7 (:REWRITE SET::NONEMPTY-MEANS-SET))
 (7 7 (:TYPE-PRESCRIPTION STRING-SETP))
 (7 7 (:TYPE-PRESCRIPTION OMAP::MAPP))
 (7 7 (:TYPE-PRESCRIPTION PFCS::CONSTREL-SETP))
 (7 7 (:TYPE-PRESCRIPTION ATOM-LISTP))
 (7 7 (:REWRITE PFIELD::TRUE-LISTP-WHEN-FE-LISTP))
 (7 7 (:REWRITE TRUE-LIST-LISTP-WHEN-NOT-CONSP))
 (7 7 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (7 7 (:REWRITE STRING-LISTP-WHEN-NOT-CONSP))
 (7 7 (:REWRITE SET::IN-SET))
 (2 2 (:REWRITE SET::IN-TAIL-OR-HEAD))
 (2 2 (:REWRITE SET::HEAD-WHEN-EMPTY))
 (1 1 (:REWRITE SET::TAIL-WHEN-EMPTY))
 )
(PFCS::LIFT-THM-OMAP-KEYS-LEMMA-INSTANCES
 (4 4 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-ATOM-LISTP))
 (3 1 (:REWRITE STRING-LISTP-WHEN-NOT-CONSP))
 )
(PFCS::TRUE-LISTP-OF-LIFT-THM-OMAP-KEYS-LEMMA-INSTANCES)
(PFCS::LIFT-THM-ASGFREE-PAIRS-AUX
 (8 4 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-ATOM-LISTP))
 (3 1 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (1 1 (:TYPE-PRESCRIPTION MEMBER-EQUAL))
 (1 1 (:REWRITE SUBSETP-MEMBER . 2))
 (1 1 (:REWRITE SUBSETP-MEMBER . 1))
 (1 1 (:REWRITE MEMBER-SELF))
 )
(PFCS::TRUE-LISTP-OF-LIFT-THM-ASGFREE-PAIRS-AUX)
(PFCS::LIFT-THM-ASGFREE-PAIRS)
(PFCS::TRUE-LISTP-OF-LIFT-THM-ASGFREE-PAIRS)
(PFCS::LIFT-THM-CALLED-LIFT-THMS
 (51 4 (:REWRITE ATOM-LISTP-WHEN-SUBSETP-EQUAL))
 (41 5 (:REWRITE SUBSETP-OF-CONS))
 (11 11 (:REWRITE SUBSETP-TRANS2))
 (11 11 (:REWRITE SUBSETP-TRANS))
 (4 4 (:REWRITE SUBSETP-MEMBER . 2))
 (4 4 (:REWRITE SUBSETP-MEMBER . 1))
 (3 1 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (2 2 (:REWRITE SUBSETP-NIL))
 (2 2 (:REWRITE ATOM-LISTP-WHEN-NOT-CONSP))
 (1 1 (:REWRITE MEMBER-SELF))
 )
(PFCS::LIFT-THM-DEFINITION-SATP-SPECIALIZED-LEMMA
 (4 4 (:REWRITE ATOM-LISTP-WHEN-SUBSETP-EQUAL))
 (2 2 (:REWRITE ATOM-LISTP-WHEN-NOT-CONSP))
 )
(PFCS::PSEUDO-EVENT-FORMP-OF-LIFT-THM-DEFINITION-SATP-SPECIALIZED-LEMMA.THM-EVENT)
(PFCS::SYMBOLP-OF-LIFT-THM-DEFINITION-SATP-SPECIALIZED-LEMMA.THM-NAME)
(PFCS::LIFT-THM-CONSTR-SATP-SPECIALIZED-LEMMA
 (4 4 (:REWRITE ATOM-LISTP-WHEN-SUBSETP-EQUAL))
 (3 1 (:REWRITE PFCS::SETP-WHEN-CONSTREL-SETP))
 (2 2 (:TYPE-PRESCRIPTION PFCS::CONSTREL-SETP))
 (2 2 (:REWRITE ATOM-LISTP-WHEN-NOT-CONSP))
 )
(PFCS::PSEUDO-EVENT-FORMP-OF-LIFT-THM-CONSTR-SATP-SPECIALIZED-LEMMA.THM-EVENT
 (10 1 (:REWRITE SYMBOLP-OF-CAR-WHEN-SYMBOL-LISTP))
 (2 2 (:TYPE-PRESCRIPTION SYMBOL-LISTP))
 (2 2 (:REWRITE SYMBOLP-WHEN-MEMBER-EQUAL-OF-SYMBOL-LISTP))
 (2 2 (:REWRITE SYMBOL-LISTP-WHEN-SUBSETP-EQUAL))
 (2 1 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 )
(PFCS::SYMBOLP-OF-LIFT-THM-CONSTR-SATP-SPECIALIZED-LEMMA.THM-NAME)
(PFCS::LIFT-THM-CONSTR-TO-DEF-SATP-SPECIALIZED-LEMMAS
 (9 3 (:REWRITE PFCS::SETP-WHEN-CONSTREL-SETP))
 (6 6 (:TYPE-PRESCRIPTION PFCS::CONSTREL-SETP))
 (4 4 (:REWRITE ATOM-LISTP-WHEN-SUBSETP-EQUAL))
 (2 2 (:REWRITE ATOM-LISTP-WHEN-NOT-CONSP))
 (1 1 (:REWRITE SET::TAIL-WHEN-EMPTY))
 (1 1 (:REWRITE SET::IN-TAIL-OR-HEAD))
 (1 1 (:REWRITE SET::HEAD-WHEN-EMPTY))
 )
(PFCS::PSEUDO-EVENT-FORM-LISTP-OF-LIFT-THM-CONSTR-TO-DEF-SATP-SPECIALIZED-LEMMAS.THM-EVENTS
 (547 7 (:DEFINITION PSEUDO-EVENT-FORM-LISTP))
 (84 7 (:REWRITE SET::SETS-ARE-TRUE-LISTS-CHEAP))
 (76 76 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-ATOM-LISTP))
 (62 7 (:REWRITE PSEUDO-EVENT-FORM-LISTP-OF-CDR-WHEN-PSEUDO-EVENT-FORM-LISTP))
 (61 7 (:REWRITE PSEUDO-EVENT-FORMP-OF-CAR-WHEN-PSEUDO-EVENT-FORM-LISTP))
 (57 29 (:REWRITE PSEUDO-EVENT-FORM-LISTP-WHEN-NOT-CONSP))
 (46 16 (:REWRITE PSEUDO-EVENT-FORMP-WHEN-MEMBER-EQUAL-OF-PSEUDO-EVENT-FORM-LISTP))
 (42 7 (:REWRITE TRUE-LISTP-WHEN-SYMBOL-LISTP-REWRITE-BACKCHAIN-1))
 (42 7 (:REWRITE TRUE-LISTP-WHEN-STRING-LISTP-REWRITE))
 (42 7 (:REWRITE SYMBOLP-OF-CAR-WHEN-SYMBOL-LISTP))
 (28 28 (:TYPE-PRESCRIPTION SYMBOL-LISTP))
 (28 28 (:REWRITE SYMBOL-LISTP-WHEN-SUBSETP-EQUAL))
 (28 7 (:REWRITE TRUE-LISTP-OF-CAR-WHEN-TRUE-LIST-LISTP))
 (24 12 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (20 12 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (18 2 (:REWRITE SUBSETP-IMPLIES-SUBSETP-CDR))
 (18 2 (:REWRITE SUBSETP-CAR-MEMBER))
 (15 15 (:REWRITE SUBSETP-TRANS2))
 (15 15 (:REWRITE SUBSETP-TRANS))
 (14 14 (:TYPE-PRESCRIPTION TRUE-LIST-LISTP))
 (14 14 (:TYPE-PRESCRIPTION STRING-LISTP))
 (14 14 (:TYPE-PRESCRIPTION SET::SETP-TYPE))
 (14 14 (:REWRITE SYMBOLP-WHEN-MEMBER-EQUAL-OF-SYMBOL-LISTP))
 (14 14 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (14 14 (:REWRITE STRING-LISTP-WHEN-SUBSETP-EQUAL))
 (14 7 (:REWRITE SETP-WHEN-STRING-SETP))
 (14 7 (:REWRITE OMAP::SETP-WHEN-MAPP))
 (14 7 (:REWRITE PFCS::SETP-WHEN-CONSTREL-SETP))
 (14 7 (:REWRITE SET::NONEMPTY-MEANS-SET))
 (14 7 (:REWRITE CONSP-OF-CAR-WHEN-ATOM-LISTP))
 (9 9 (:TYPE-PRESCRIPTION MEMBER-EQUAL))
 (9 1 (:REWRITE SUBSETP-OF-CONS))
 (7 7 (:TYPE-PRESCRIPTION STRING-SETP))
 (7 7 (:TYPE-PRESCRIPTION OMAP::MAPP))
 (7 7 (:TYPE-PRESCRIPTION PFCS::CONSTREL-SETP))
 (7 7 (:TYPE-PRESCRIPTION ATOM-LISTP))
 (7 7 (:REWRITE PFIELD::TRUE-LISTP-WHEN-FE-LISTP))
 (7 7 (:REWRITE TRUE-LIST-LISTP-WHEN-NOT-CONSP))
 (7 7 (:REWRITE STRING-LISTP-WHEN-NOT-CONSP))
 (7 7 (:REWRITE SET::IN-SET))
 (4 4 (:REWRITE SUBSETP-MEMBER . 2))
 (4 4 (:REWRITE SUBSETP-MEMBER . 1))
 (2 2 (:REWRITE SET::TAIL-WHEN-EMPTY))
 (1 1 (:REWRITE SET::IN-TAIL-OR-HEAD))
 (1 1 (:REWRITE SET::HEAD-WHEN-EMPTY))
 )
(PFCS::SYMBOL-LISTP-OF-LIFT-THM-CONSTR-TO-DEF-SATP-SPECIALIZED-LEMMAS.THM-NAMES
 (38 22 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-ATOM-LISTP))
 (22 8 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (14 2 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (14 2 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (9 9 (:TYPE-PRESCRIPTION MEMBER-EQUAL))
 (9 1 (:REWRITE SUBSETP-OF-CONS))
 (5 5 (:REWRITE SUBSETP-TRANS2))
 (5 5 (:REWRITE SUBSETP-TRANS))
 (5 5 (:REWRITE SUBSETP-MEMBER . 2))
 (5 5 (:REWRITE SUBSETP-MEMBER . 1))
 (2 2 (:REWRITE MEMBER-SELF))
 (1 1 (:REWRITE SET::TAIL-WHEN-EMPTY))
 (1 1 (:REWRITE SET::IN-TAIL-OR-HEAD))
 (1 1 (:REWRITE SET::HEAD-WHEN-EMPTY))
 )
(PFCS::LIFT-THM-TYPE-PRESCRIPTIONS-FOR-CALLED-PREDS
 (4 4 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-ATOM-LISTP))
 (3 1 (:REWRITE STRING-LISTP-WHEN-NOT-CONSP))
 (1 1 (:REWRITE SUBSETP-TRANS2))
 (1 1 (:REWRITE SUBSETP-TRANS))
 )
(PFCS::TRUE-LISTP-OF-LIFT-THM-TYPE-PRESCRIPTIONS-FOR-CALLED-PREDS)
(PFCS::LIFT-THM
 (108 44 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-ATOM-LISTP))
 (98 14 (:REWRITE STRING-LISTP-WHEN-SUBSETP-EQUAL))
 (81 8 (:REWRITE SYMBOL-LISTP-WHEN-SUBSETP-EQUAL))
 (72 9 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (50 1 (:REWRITE SUBSETP-APPEND1))
 (49 4 (:REWRITE ATOM-LISTP-WHEN-SUBSETP-EQUAL))
 (48 48 (:TYPE-PRESCRIPTION MEMBER-EQUAL))
 (48 9 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (39 5 (:REWRITE SUBSETP-OF-CONS))
 (36 16 (:REWRITE SUBSETP-TRANS2))
 (33 6 (:REWRITE STRING-LISTP-WHEN-NOT-CONSP))
 (24 24 (:REWRITE SUBSETP-MEMBER . 2))
 (24 24 (:REWRITE SUBSETP-MEMBER . 1))
 (20 20 (:TYPE-PRESCRIPTION SUBSETP-EQUAL))
 (18 6 (:REWRITE SYMBOLP-WHEN-MEMBER-EQUAL-OF-SYMBOL-LISTP))
 (18 2 (:REWRITE PFCS::CONSP-OF-NAME-LIST-TO-SYMBOL-LIST))
 (16 16 (:REWRITE SUBSETP-TRANS))
 (10 1 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (6 1 (:REWRITE PFCS::NAME-LIST-TO-SYMBOL-LIST-WHEN-NOT-CONSP))
 (3 1 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (2 2 (:REWRITE SUBSETP-NIL))
 (2 2 (:REWRITE MEMBER-SELF))
 (2 2 (:REWRITE ATOM-LISTP-WHEN-NOT-CONSP))
 )
(PFCS::PSEUDO-EVENT-FORMP-OF-LIFT-THM.EVENT
 (231 63 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (88 88 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-ATOM-LISTP))
 (39 39 (:TYPE-PRESCRIPTION PFCS::DEFINITION->PARA$INLINE))
 (30 6 (:REWRITE PFCS::CONSP-OF-NAME-LIST-TO-SYMBOL-LIST))
 (18 18 (:TYPE-PRESCRIPTION PFCS::SESEM-GEN-FEP-TERMS))
 (18 18 (:TYPE-PRESCRIPTION PFCS::LIFT-THM-CALLED-LIFT-THMS))
 (18 3 (:REWRITE PFCS::NAME-LIST-TO-SYMBOL-LIST-WHEN-NOT-CONSP))
 (18 3 (:REWRITE PFCS::EXPRESSION-VAR-LIST-WHEN-NOT-CONSP))
 (12 12 (:TYPE-PRESCRIPTION PFCS::TRUE-LISTP-OF-NAME-LIST-TO-SYMBOL-LIST))
 (12 12 (:TYPE-PRESCRIPTION PFCS::LIFT-THM-DEF-HYPS))
 (12 1 (:REWRITE SET::SETS-ARE-TRUE-LISTS-CHEAP))
 (9 9 (:TYPE-PRESCRIPTION PFCS::LIFT-THM-TYPE-PRESCRIPTIONS-FOR-CALLED-PREDS))
 (8 1 (:REWRITE SYMBOLP-OF-CAR-WHEN-SYMBOL-LISTP))
 (6 1 (:REWRITE TRUE-LISTP-WHEN-SYMBOL-LISTP-REWRITE-BACKCHAIN-1))
 (6 1 (:REWRITE TRUE-LISTP-WHEN-STRING-LISTP-REWRITE))
 (4 4 (:TYPE-PRESCRIPTION SYMBOL-LISTP))
 (4 4 (:REWRITE SYMBOL-LISTP-WHEN-SUBSETP-EQUAL))
 (4 2 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (2 2 (:TYPE-PRESCRIPTION STRING-LISTP))
 (2 2 (:TYPE-PRESCRIPTION SET::SETP-TYPE))
 (2 2 (:REWRITE SYMBOLP-WHEN-MEMBER-EQUAL-OF-SYMBOL-LISTP))
 (2 2 (:REWRITE STRING-LISTP-WHEN-SUBSETP-EQUAL))
 (2 1 (:REWRITE SETP-WHEN-STRING-SETP))
 (2 1 (:REWRITE OMAP::SETP-WHEN-MAPP))
 (2 1 (:REWRITE PFCS::SETP-WHEN-CONSTREL-SETP))
 (2 1 (:REWRITE SET::NONEMPTY-MEANS-SET))
 (1 1 (:TYPE-PRESCRIPTION STRING-SETP))
 (1 1 (:TYPE-PRESCRIPTION OMAP::MAPP))
 (1 1 (:TYPE-PRESCRIPTION SET::EMPTY-TYPE))
 (1 1 (:TYPE-PRESCRIPTION PFCS::CONSTREL-SETP))
 (1 1 (:REWRITE PFIELD::TRUE-LISTP-WHEN-FE-LISTP))
 (1 1 (:REWRITE STRING-LISTP-WHEN-NOT-CONSP))
 (1 1 (:REWRITE SET::IN-SET))
 )
(PFCS::TRUE-LISTP-OF-LIFT-THM.DEF-HYPS
 (77 21 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (28 28 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-ATOM-LISTP))
 (13 13 (:TYPE-PRESCRIPTION PFCS::DEFINITION->PARA$INLINE))
 (12 1 (:REWRITE SET::SETS-ARE-TRUE-LISTS-CHEAP))
 (10 2 (:REWRITE PFCS::CONSP-OF-NAME-LIST-TO-SYMBOL-LIST))
 (6 6 (:TYPE-PRESCRIPTION PFCS::SESEM-GEN-FEP-TERMS))
 (6 6 (:TYPE-PRESCRIPTION PFCS::LIFT-THM-CALLED-LIFT-THMS))
 (6 1 (:REWRITE TRUE-LISTP-WHEN-SYMBOL-LISTP-REWRITE-BACKCHAIN-1))
 (6 1 (:REWRITE TRUE-LISTP-WHEN-STRING-LISTP-REWRITE))
 (6 1 (:REWRITE PFCS::NAME-LIST-TO-SYMBOL-LIST-WHEN-NOT-CONSP))
 (6 1 (:REWRITE PFCS::EXPRESSION-VAR-LIST-WHEN-NOT-CONSP))
 (4 4 (:TYPE-PRESCRIPTION PFCS::TRUE-LISTP-OF-NAME-LIST-TO-SYMBOL-LIST))
 (4 4 (:TYPE-PRESCRIPTION PFCS::LIFT-THM-DEF-HYPS))
 (3 3 (:TYPE-PRESCRIPTION PFCS::LIFT-THM-TYPE-PRESCRIPTIONS-FOR-CALLED-PREDS))
 (2 2 (:TYPE-PRESCRIPTION SYMBOL-LISTP))
 (2 2 (:TYPE-PRESCRIPTION STRING-LISTP))
 (2 2 (:TYPE-PRESCRIPTION SET::SETP-TYPE))
 (2 2 (:REWRITE SYMBOL-LISTP-WHEN-SUBSETP-EQUAL))
 (2 2 (:REWRITE STRING-LISTP-WHEN-SUBSETP-EQUAL))
 (2 1 (:REWRITE SETP-WHEN-STRING-SETP))
 (2 1 (:REWRITE OMAP::SETP-WHEN-MAPP))
 (2 1 (:REWRITE PFCS::SETP-WHEN-CONSTREL-SETP))
 (2 1 (:REWRITE SET::NONEMPTY-MEANS-SET))
 (1 1 (:TYPE-PRESCRIPTION STRING-SETP))
 (1 1 (:TYPE-PRESCRIPTION OMAP::MAPP))
 (1 1 (:TYPE-PRESCRIPTION SET::EMPTY-TYPE))
 (1 1 (:TYPE-PRESCRIPTION PFCS::CONSTREL-SETP))
 (1 1 (:REWRITE PFIELD::TRUE-LISTP-WHEN-FE-LISTP))
 (1 1 (:REWRITE SYMBOL-LISTP-WHEN-NOT-CONSP))
 (1 1 (:REWRITE STRING-LISTP-WHEN-NOT-CONSP))
 (1 1 (:REWRITE SET::IN-SET))
 )
(PFCS::LIFT-TABLE-ADD)
(PFCS::PSEUDO-EVENT-FORMP-OF-LIFT-TABLE-ADD)
(PFCS::LIFT-FN)
(PFCS::PSEUDO-EVENT-FORMP-OF-LIFT-FN
 (3 1 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (2 2 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-ATOM-LISTP))
 (1 1 (:REWRITE APPEND-ATOM-UNDER-LIST-EQUIV))
 )