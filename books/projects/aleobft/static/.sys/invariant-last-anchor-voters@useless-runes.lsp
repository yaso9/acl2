(ALEOBFT-STATIC::VALIDATOR-LAST-ANCHOR-VOTERS-P)
(ALEOBFT-STATIC::BOOLEANP-OF-VALIDATOR-LAST-ANCHOR-VOTERS-P)
(ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P)
(ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-NECC)
(ALEOBFT-STATIC::BOOLEANP-OF-SYSTEM-LAST-ANCHOR-VOTERS-P)
(ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P
 (15 5 (:REWRITE ALEOBFT-STATIC::VALIDATOR-INIT-WHEN-SYSTEM-INITP))
 (14 14 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 (14 8 (:REWRITE SET::IN-TAIL))
 (12 7 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (9 7 (:REWRITE SET::NEVER-IN-EMPTY))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE ALEOBFT-STATIC::SYSTEM-LAST-IS-EVEN-P-WHEN-SYSTEM-STATE-INITP))
 (3 1 (:REWRITE ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-PRESENT-P-WHEN-SYSTEM-STATE-INITP))
 (3 1 (:REWRITE SETP-WHEN-POS-SETP))
 (3 1 (:REWRITE ALEOBFT-STATIC::SETP-WHEN-MESSAGE-SETP))
 (3 1 (:REWRITE ALEOBFT-STATIC::SETP-WHEN-CERTIFICATE-SETP))
 (3 1 (:REWRITE ALEOBFT-STATIC::SETP-WHEN-ADDRESS+POS-SETP))
 (2 2 (:TYPE-PRESCRIPTION POS-SETP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::MESSAGE-SETP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::CERTIFICATE-SETP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::ADDRESS+POS-SETP))
 (1 1 (:REWRITE ALEOBFT-STATIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 )
(ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-WHEN-SYSTEM-STATE-INITP
 (14 2 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (10 3 (:REWRITE SET::IN-TAIL))
 (3 3 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::GET-VALIDATOR-STATE))
 (3 3 (:REWRITE-QUOTED-CONSTANT ALEOBFT-STATIC::CERTIFICATE-SET-FIX-UNDER-CERTIFICATE-SET-EQUIV))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (1 1 (:REWRITE-QUOTED-CONSTANT POS-FIX-UNDER-POS-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT ALEOBFT-STATIC::BLOCK-LIST-FIX-UNDER-BLOCK-LIST-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT ALEOBFT-STATIC::ADDRESS+POS-SET-FIX-UNDER-ADDRESS+POS-SET-EQUIV))
 )
(ALEOBFT-STATIC::VALIDATOR-LAST-ANCHOR-VOTERS-P-OF-CREATE-CERTIFICATE-NEXT
 (93 41 (:REWRITE ALEOBFT-STATIC::VALIDATOR-INIT-WHEN-SYSTEM-INITP))
 (81 25 (:REWRITE SET::IN-TAIL))
 (52 52 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 (46 4 (:REWRITE SET::INSERT-IDENTITY))
 (42 42 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (36 8 (:REWRITE ALEOBFT-STATIC::CERTIFICATEP-WHEN-CERTIFICATE-OPTIONP))
 (27 9 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (26 10 (:REWRITE SET::NEVER-IN-EMPTY))
 (24 4 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (24 4 (:REWRITE ALEOBFT-STATIC::CERTIFICATE-OPTIONP-WHEN-CERTIFICATEP))
 (12 12 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::CERTIFICATE-OPTIONP))
 (12 4 (:REWRITE SET::INSERT-WHEN-EMPTYP))
 (8 8 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::GET-VALIDATOR-STATE))
 (8 8 (:REWRITE ALEOBFT-STATIC::CERTIFICATEP-WHEN-IN-CERTIFICATE-SETP-BINDS-FREE-X))
 (6 1 (:REWRITE ALEOBFT-STATIC::VALIDATOR-STATE->DAG-OF-CREATE-CERTIFICATE-NEXT-SAME))
 (2 2 (:LINEAR ALEOBFT-STATIC::PATH-TO-AUTHOR+ROUND-ROUND-LTE))
 )
(ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-OF-CREATE-CERTIFICATE-NEXT
 (10 10 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 (9 3 (:REWRITE ALEOBFT-STATIC::VALIDATOR-INIT-WHEN-SYSTEM-INITP))
 (9 2 (:REWRITE ALEOBFT-STATIC::CERTIFICATEP-WHEN-CERTIFICATE-OPTIONP))
 (8 2 (:REWRITE SET::IN-TAIL))
 (6 2 (:REWRITE ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-WHEN-SYSTEM-STATE-INITP))
 (6 1 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (6 1 (:REWRITE ALEOBFT-STATIC::CERTIFICATE-OPTIONP-WHEN-CERTIFICATEP))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 3 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::CERTIFICATE-OPTIONP))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::GET-VALIDATOR-STATE))
 (2 2 (:REWRITE ALEOBFT-STATIC::CERTIFICATEP-WHEN-IN-CERTIFICATE-SETP-BINDS-FREE-X))
 )
(ALEOBFT-STATIC::VALIDATOR-LAST-ANCHOR-VOTERS-P-OF-RECEIVE-CERTIFICATE-NEXT
 (29 13 (:REWRITE ALEOBFT-STATIC::VALIDATOR-INIT-WHEN-SYSTEM-INITP))
 (16 16 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 (8 2 (:REWRITE SET::IN-TAIL))
 (6 1 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::GET-VALIDATOR-STATE))
 )
(ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-OF-RECEIVE-CERTIFICATE-NEXT
 (10 10 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 (9 3 (:REWRITE ALEOBFT-STATIC::VALIDATOR-INIT-WHEN-SYSTEM-INITP))
 (8 2 (:REWRITE SET::IN-TAIL))
 (6 2 (:REWRITE ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-WHEN-SYSTEM-STATE-INITP))
 (6 1 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::GET-VALIDATOR-STATE))
 )
(ALEOBFT-STATIC::VALIDATOR-LAST-ANCHOR-VOTERS-P-OF-STORE-CERTIFICATE-NEXT
 (93 41 (:REWRITE ALEOBFT-STATIC::VALIDATOR-INIT-WHEN-SYSTEM-INITP))
 (81 25 (:REWRITE SET::IN-TAIL))
 (52 52 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 (46 4 (:REWRITE SET::INSERT-IDENTITY))
 (42 42 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (36 8 (:REWRITE ALEOBFT-STATIC::CERTIFICATEP-WHEN-CERTIFICATE-OPTIONP))
 (27 9 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (26 10 (:REWRITE SET::NEVER-IN-EMPTY))
 (24 4 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (24 4 (:REWRITE ALEOBFT-STATIC::CERTIFICATE-OPTIONP-WHEN-CERTIFICATEP))
 (12 12 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::CERTIFICATE-OPTIONP))
 (12 4 (:REWRITE SET::INSERT-WHEN-EMPTYP))
 (8 8 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::GET-VALIDATOR-STATE))
 (8 8 (:REWRITE ALEOBFT-STATIC::CERTIFICATEP-WHEN-IN-CERTIFICATE-SETP-BINDS-FREE-X))
 (2 2 (:LINEAR ALEOBFT-STATIC::PATH-TO-AUTHOR+ROUND-ROUND-LTE))
 (1 1 (:REWRITE ALEOBFT-STATIC::VALIDATOR-STATE->DAG-OF-STORE-CERTIFICATE-NEXT-SAME))
 )
(ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-OF-STORE-CERTIFICATE-NEXT
 (10 10 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 (9 3 (:REWRITE ALEOBFT-STATIC::VALIDATOR-INIT-WHEN-SYSTEM-INITP))
 (9 2 (:REWRITE ALEOBFT-STATIC::CERTIFICATEP-WHEN-CERTIFICATE-OPTIONP))
 (8 2 (:REWRITE SET::IN-TAIL))
 (6 2 (:REWRITE ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-WHEN-SYSTEM-STATE-INITP))
 (6 1 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (6 1 (:REWRITE ALEOBFT-STATIC::CERTIFICATE-OPTIONP-WHEN-CERTIFICATEP))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 3 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::CERTIFICATE-OPTIONP))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::GET-VALIDATOR-STATE))
 (2 2 (:REWRITE ALEOBFT-STATIC::CERTIFICATEP-WHEN-IN-CERTIFICATE-SETP-BINDS-FREE-X))
 )
(ALEOBFT-STATIC::VALIDATOR-LAST-ANCHOR-VOTERS-P-OF-ADVANCE-ROUND-NEXT
 (29 13 (:REWRITE ALEOBFT-STATIC::VALIDATOR-INIT-WHEN-SYSTEM-INITP))
 (16 16 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 (8 2 (:REWRITE SET::IN-TAIL))
 (6 1 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::GET-VALIDATOR-STATE))
 )
(ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-OF-ADVANCE-ROUND-NEXT
 (10 10 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 (9 3 (:REWRITE ALEOBFT-STATIC::VALIDATOR-INIT-WHEN-SYSTEM-INITP))
 (8 2 (:REWRITE SET::IN-TAIL))
 (6 2 (:REWRITE ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-WHEN-SYSTEM-STATE-INITP))
 (6 1 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::GET-VALIDATOR-STATE))
 )
(ALEOBFT-STATIC::VALIDATOR-LAST-ANCHOR-VOTERS-P-OF-COMMIT-ANCHORS-NEXT
 (160 84 (:REWRITE ALEOBFT-STATIC::VALIDATOR-INIT-WHEN-SYSTEM-INITP))
 (96 30 (:REWRITE SET::IN-TAIL))
 (76 76 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 (74 19 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (33 11 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (30 30 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (12 4 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-OF-COMMIT-ANCHORS-NEXT
 (10 10 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 (9 3 (:REWRITE ALEOBFT-STATIC::VALIDATOR-INIT-WHEN-SYSTEM-INITP))
 (8 2 (:REWRITE SET::IN-TAIL))
 (6 2 (:REWRITE ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-WHEN-SYSTEM-STATE-INITP))
 (6 1 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::GET-VALIDATOR-STATE))
 )
(ALEOBFT-STATIC::VALIDATOR-LAST-ANCHOR-VOTERS-P-OF-TIMER-EXPIRES-NEXT
 (29 13 (:REWRITE ALEOBFT-STATIC::VALIDATOR-INIT-WHEN-SYSTEM-INITP))
 (16 16 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 (8 2 (:REWRITE SET::IN-TAIL))
 (6 1 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::GET-VALIDATOR-STATE))
 )
(ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-OF-TIMER-EXPIRES-NEXT
 (10 10 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 (9 3 (:REWRITE ALEOBFT-STATIC::VALIDATOR-INIT-WHEN-SYSTEM-INITP))
 (8 2 (:REWRITE SET::IN-TAIL))
 (6 2 (:REWRITE ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-WHEN-SYSTEM-STATE-INITP))
 (6 1 (:REWRITE ALEOBFT-STATIC::IN-CORRECT-VALIDATOR-ADDRESESS-WHEN-GET-VALIDATOR-STATE))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::GET-VALIDATOR-STATE))
 )
(ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-OF-EVENT-NEXT
 (3 1 (:REWRITE ALEOBFT-STATIC::SYSTEM-LAST-ANCHOR-VOTERS-P-WHEN-SYSTEM-STATE-INITP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-STATIC::SYSTEM-STATE-INITP))
 )
