(ALEOBFT-DYNAMIC::ADVANCE-ROUND-POSSIBLEP
 (19 7 (:REWRITE SET::IN-TAIL))
 (9 5 (:REWRITE SET::NEVER-IN-EMPTY))
 (8 8 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (6 2 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (6 2 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-MESSAGE-SETP))
 (6 2 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS+POS-SETP))
 (4 4 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::MESSAGE-SETP))
 (4 4 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SETP))
 (4 4 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (3 1 (:REWRITE SETP-WHEN-POS-SETP))
 (2 2 (:TYPE-PRESCRIPTION POS-SETP))
 (2 2 (:REWRITE POSP-WHEN-IN-POS-SETP-BINDS-FREE-X))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::BOOLEANP-OF-ADVANCE-ROUND-POSSIBLEP)
(ALEOBFT-DYNAMIC::ADVANCE-ROUND-POSSIBLEP-OF-ADDRESS-FIX-VAL
 (11 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (9 3 (:REWRITE SET::IN-TAIL))
 (5 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (4 4 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (4 2 (:REWRITE SET::NEVER-IN-EMPTY))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::ADVANCE-ROUND-POSSIBLEP-ADDRESS-EQUIV-CONGRUENCE-ON-VAL)
(ALEOBFT-DYNAMIC::ADVANCE-ROUND-POSSIBLEP-OF-SYSTEM-STATE-FIX-SYSTATE
 (11 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (9 3 (:REWRITE SET::IN-TAIL))
 (5 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (4 2 (:REWRITE SET::NEVER-IN-EMPTY))
 (3 3 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE ALEOBFT-DYNAMIC::SYSTEM-STATE-FIX-WHEN-SYSTEM-STATEP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::SYSTEM-STATEP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (2 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::ADVANCE-ROUND-POSSIBLEP-SYSTEM-STATE-EQUIV-CONGRUENCE-ON-SYSTATE)
(ALEOBFT-DYNAMIC::ADVANCE-ROUND-NEXT
 (147 35 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (112 14 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (71 23 (:REWRITE SET::IN-TAIL))
 (35 35 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (32 32 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (31 15 (:REWRITE SET::NEVER-IN-EMPTY))
 (28 28 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (24 8 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 )
(ALEOBFT-DYNAMIC::SYSTEM-STATEP-OF-ADVANCE-ROUND-NEXT)
(ALEOBFT-DYNAMIC::ALL-ADDRESSES-OF-ADVANCE-ROUND-NEXT
 (10 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (8 2 (:REWRITE SET::IN-TAIL))
 (5 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 3 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (2 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::CORRECT-ADDRESSES-OF-ADVANCE-ROUND-NEXT
 (10 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (8 2 (:REWRITE SET::IN-TAIL))
 (5 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 3 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (2 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::FAULTY-ADDRESSES-OF-ADVANCE-ROUND-NEXT
 (10 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (8 2 (:REWRITE SET::IN-TAIL))
 (5 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 3 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (2 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-STATE->DAG-OF-ADVANCE-ROUND-NEXT
 (374 50 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (231 7 (:REWRITE ALEOBFT-DYNAMIC::GET-VALIDATOR-STATE-OF-UPDATE-VALIDATOR-STATE-DIFF))
 (180 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (173 173 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-SET-FIX))
 (108 108 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (72 72 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (72 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (64 16 (:REWRITE SET::IN-TAIL))
 (36 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (36 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 (32 32 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (24 8 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (24 8 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:LINEAR SET::SUBSET-CARDINALITY))
 (2 2 (:LINEAR SET::PROPER-SUBSET-CARDINALITY))
 (1 1 (:REWRITE-QUOTED-CONSTANT POS-FIX-UNDER-POS-EQUIV))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-STATE->BUFFER-OF-ADVANCE-ROUND-NEXT
 (374 50 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (231 7 (:REWRITE ALEOBFT-DYNAMIC::GET-VALIDATOR-STATE-OF-UPDATE-VALIDATOR-STATE-DIFF))
 (180 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (173 173 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-SET-FIX))
 (108 108 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (72 72 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (72 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (64 16 (:REWRITE SET::IN-TAIL))
 (36 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (36 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 (32 32 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (24 8 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (24 8 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:LINEAR SET::SUBSET-CARDINALITY))
 (2 2 (:LINEAR SET::PROPER-SUBSET-CARDINALITY))
 (1 1 (:REWRITE-QUOTED-CONSTANT POS-FIX-UNDER-POS-EQUIV))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-STATE->ENDORSED-OF-ADVANCE-ROUND-NEXT
 (374 50 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (231 7 (:REWRITE ALEOBFT-DYNAMIC::GET-VALIDATOR-STATE-OF-UPDATE-VALIDATOR-STATE-DIFF))
 (180 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (173 173 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SET-FIX))
 (108 108 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (72 72 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (72 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (64 16 (:REWRITE SET::IN-TAIL))
 (36 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (36 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 (32 32 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (24 8 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (24 8 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:LINEAR SET::SUBSET-CARDINALITY))
 (2 2 (:LINEAR SET::PROPER-SUBSET-CARDINALITY))
 (1 1 (:REWRITE-QUOTED-CONSTANT POS-FIX-UNDER-POS-EQUIV))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-STATE->LAST-OF-ADVANCE-ROUND-NEXT
 (374 50 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (231 7 (:REWRITE ALEOBFT-DYNAMIC::GET-VALIDATOR-STATE-OF-UPDATE-VALIDATOR-STATE-DIFF))
 (180 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (108 108 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (72 72 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (72 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (64 16 (:REWRITE SET::IN-TAIL))
 (36 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (36 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 (32 32 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (24 8 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (24 8 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:LINEAR SET::SUBSET-CARDINALITY))
 (2 2 (:LINEAR SET::PROPER-SUBSET-CARDINALITY))
 (1 1 (:REWRITE-QUOTED-CONSTANT POS-FIX-UNDER-POS-EQUIV))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-STATE->BLOCKCHAIN-OF-ADVANCE-ROUND-NEXT
 (374 50 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (231 7 (:REWRITE ALEOBFT-DYNAMIC::GET-VALIDATOR-STATE-OF-UPDATE-VALIDATOR-STATE-DIFF))
 (180 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (108 108 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (72 72 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (72 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (64 16 (:REWRITE SET::IN-TAIL))
 (36 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (36 36 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 (32 32 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (24 8 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (24 8 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:LINEAR SET::SUBSET-CARDINALITY))
 (2 2 (:LINEAR SET::PROPER-SUBSET-CARDINALITY))
 (1 1 (:REWRITE-QUOTED-CONSTANT POS-FIX-UNDER-POS-EQUIV))
 )
(ALEOBFT-DYNAMIC::GET-NETWORK-STATE-OF-ADVANCE-ROUND-NEXT)
(ALEOBFT-DYNAMIC::ADVANCE-ROUND-NEXT-OF-ADDRESS-FIX-VAL
 (10 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (5 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (3 3 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (2 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::ADVANCE-ROUND-NEXT-ADDRESS-EQUIV-CONGRUENCE-ON-VAL)
(ALEOBFT-DYNAMIC::ADVANCE-ROUND-NEXT-OF-SYSTEM-STATE-FIX-SYSTATE
 (3 1 (:REWRITE ALEOBFT-DYNAMIC::SYSTEM-STATE-FIX-WHEN-SYSTEM-STATEP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::SYSTEM-STATEP))
 )
(ALEOBFT-DYNAMIC::ADVANCE-ROUND-NEXT-SYSTEM-STATE-EQUIV-CONGRUENCE-ON-SYSTATE)
