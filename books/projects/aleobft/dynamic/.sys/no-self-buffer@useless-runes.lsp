(ALEOBFT-DYNAMIC::NO-SELF-BUFFER-P)
(ALEOBFT-DYNAMIC::NO-SELF-BUFFER-P-NECC)
(ALEOBFT-DYNAMIC::BOOLEANP-OF-NO-SELF-BUFFER-P)
(ALEOBFT-DYNAMIC::NO-SELF-BUFFER-P
 (10 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (9 3 (:REWRITE SET::IN-TAIL))
 (8 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (4 2 (:REWRITE SET::NEVER-IN-EMPTY))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SETP-WHEN-POS-SETP))
 (3 1 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-MESSAGE-SETP))
 (3 1 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-CERTIFICATE-SETP))
 (3 1 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS+POS-SETP))
 (2 2 (:TYPE-PRESCRIPTION POS-SETP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::MESSAGE-SETP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SETP))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::NO-SELF-BUFFER-P-NECC-FIXING)
(ALEOBFT-DYNAMIC::NO-SELF-BUFFER-P-WHEN-INIT
 (8 2 (:REWRITE SET::IN-TAIL))
 (3 3 (:REWRITE-QUOTED-CONSTANT ALEOBFT-DYNAMIC::CERTIFICATE-SET-FIX-UNDER-CERTIFICATE-SET-EQUIV))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (3 1 (:REWRITE SET::EMPTYP-SUBSET-2))
 (3 1 (:REWRITE SET::EMPTYP-SUBSET))
 (1 1 (:REWRITE-QUOTED-CONSTANT POS-FIX-UNDER-POS-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT ALEOBFT-DYNAMIC::BLOCK-LIST-FIX-UNDER-BLOCK-LIST-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT ALEOBFT-DYNAMIC::ADDRESS+POS-SET-FIX-UNDER-ADDRESS+POS-SET-EQUIV))
 (1 1 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 )
(ALEOBFT-DYNAMIC::NO-SELF-BUFFER-P-OF-CREATE-CERTIFICATE-NEXT
 (72 18 (:REWRITE SET::IN-TAIL))
 (36 36 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (27 9 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (27 9 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::NO-SELF-BUFFER-P-OF-RECEIVE-CERTIICATE-NEXT
 (536 140 (:REWRITE SET::IN-TAIL))
 (272 56 (:LINEAR SET::PROPER-SUBSET-CARDINALITY))
 (190 66 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (156 52 (:REWRITE SET::EMPTYP-SUBSET))
 (150 50 (:REWRITE SET::NEVER-IN-EMPTY))
 (144 52 (:REWRITE SET::EMPTYP-SUBSET-2))
 (76 19 (:REWRITE ALEOBFT-DYNAMIC::MESSAGE-FIX-WHEN-MESSAGEP))
 (52 52 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (46 46 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (40 8 (:REWRITE SET::INSERT-IDENTITY))
 (38 38 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::MESSAGEP))
 (24 8 (:REWRITE SET::INSERT-WHEN-EMPTYP))
 (19 19 (:REWRITE ALEOBFT-DYNAMIC::MESSAGEP-WHEN-IN-MESSAGE-SETP-BINDS-FREE-X))
 (12 12 (:REWRITE-QUOTED-CONSTANT POS-FIX-UNDER-POS-EQUIV))
 (4 4 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::NO-SELF-BUFFER-P-OF-STORE-CERTIFICATE-NEXT
 (148 40 (:REWRITE SET::IN-TAIL))
 (51 17 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (40 8 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (36 4 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATE-FIX-WHEN-CERTIFICATEP))
 (20 4 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-CERTIFICATE-OPTIONP))
 (16 16 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (16 8 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (12 12 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATEP))
 (8 8 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP))
 (8 8 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (8 4 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP-WHEN-CERTIFICATEP))
 (4 4 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-IN-CERTIFICATE-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::NO-SELF-BUFFER-P-OF-ADVANCE-ROUND-NEXT
 (72 18 (:REWRITE SET::IN-TAIL))
 (36 36 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (27 9 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (27 9 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::NO-SELF-BUFFER-P-OF-COMMIT-ANCHORS-NEXT
 (72 18 (:REWRITE SET::IN-TAIL))
 (36 36 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (27 9 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (27 9 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::NO-SELF-BUFFER-P-OF-TIMER-EXPIRES-NEXT
 (72 18 (:REWRITE SET::IN-TAIL))
 (36 36 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (27 9 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (27 9 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::NO-SELF-BUFFER-P-OF-EVENT-NEXT
 (3 1 (:REWRITE ALEOBFT-DYNAMIC::NO-SELF-MESSAGES-P-WHEN-INIT))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::SYSTEM-INITP))
 )
