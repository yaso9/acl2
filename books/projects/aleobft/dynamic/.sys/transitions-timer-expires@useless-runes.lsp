(ALEOBFT-DYNAMIC::TIMER-EXPIRES-POSSIBLEP
 (18 6 (:REWRITE SET::IN-TAIL))
 (8 8 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (8 4 (:REWRITE SET::NEVER-IN-EMPTY))
 (6 2 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (4 4 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (3 1 (:REWRITE SETP-WHEN-POS-SETP))
 (3 1 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-MESSAGE-SETP))
 (3 1 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-CERTIFICATE-SETP))
 (3 1 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS+POS-SETP))
 (2 2 (:TYPE-PRESCRIPTION POS-SETP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::MESSAGE-SETP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-SETP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SETP))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::BOOLEANP-OF-TIMER-EXPIRES-POSSIBLEP)
(ALEOBFT-DYNAMIC::TIMER-EXPIRES-POSSIBLEP-OF-ADDRESS-FIX-VAL
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
(ALEOBFT-DYNAMIC::TIMER-EXPIRES-POSSIBLEP-ADDRESS-EQUIV-CONGRUENCE-ON-VAL)
(ALEOBFT-DYNAMIC::TIMER-EXPIRES-POSSIBLEP-OF-SYSTEM-STATE-FIX-SYSTATE
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
(ALEOBFT-DYNAMIC::TIMER-EXPIRES-POSSIBLEP-SYSTEM-STATE-EQUIV-CONGRUENCE-ON-SYSTATE)
(ALEOBFT-DYNAMIC::TIMER-EXPIRES-NEXT
 (21 5 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (17 5 (:REWRITE SET::IN-TAIL))
 (16 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (8 8 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (7 3 (:REWRITE SET::NEVER-IN-EMPTY))
 (6 2 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (5 5 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (4 4 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 )
(ALEOBFT-DYNAMIC::SYSTEM-STATEP-OF-TIMER-EXPIRES-NEXT)
(ALEOBFT-DYNAMIC::ALL-ADDRESSES-OF-TIMER-EXPIRES-NEXT
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
(ALEOBFT-DYNAMIC::CORRECT-ADDRESSES-OF-TIMER-EXPIRES-NEXT
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
(ALEOBFT-DYNAMIC::FAULTY-ADDRESSES-OF-TIMER-EXPIRES-NEXT
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
(ALEOBFT-DYNAMIC::VALIDATOR-STATE->DAG-OF-TIMER-EXPIRES-NEXT
 (62 8 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (33 1 (:REWRITE ALEOBFT-DYNAMIC::GET-VALIDATOR-STATE-OF-UPDATE-VALIDATOR-STATE-DIFF))
 (30 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (21 21 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-SET-FIX))
 (18 18 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (16 4 (:REWRITE SET::IN-TAIL))
 (12 12 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (12 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (8 8 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (6 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (6 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 (6 2 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (6 2 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-STATE->BUFFER-OF-TIMER-EXPIRES-NEXT
 (62 8 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (33 1 (:REWRITE ALEOBFT-DYNAMIC::GET-VALIDATOR-STATE-OF-UPDATE-VALIDATOR-STATE-DIFF))
 (30 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (21 21 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-SET-FIX))
 (18 18 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (16 4 (:REWRITE SET::IN-TAIL))
 (12 12 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (12 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (8 8 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (6 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (6 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 (6 2 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (6 2 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-STATE->ENDORSED-OF-TIMER-EXPIRES-NEXT
 (62 8 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (33 1 (:REWRITE ALEOBFT-DYNAMIC::GET-VALIDATOR-STATE-OF-UPDATE-VALIDATOR-STATE-DIFF))
 (30 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (21 21 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SET-FIX))
 (18 18 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (16 4 (:REWRITE SET::IN-TAIL))
 (12 12 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (12 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (8 8 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (6 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (6 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 (6 2 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (6 2 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-STATE->LAST-OF-TIMER-EXPIRES-NEXT
 (62 8 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (33 1 (:REWRITE ALEOBFT-DYNAMIC::GET-VALIDATOR-STATE-OF-UPDATE-VALIDATOR-STATE-DIFF))
 (30 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (18 18 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (16 4 (:REWRITE SET::IN-TAIL))
 (12 12 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (12 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (8 8 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (6 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (6 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 (6 2 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (6 2 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::VALIDATOR-STATE->BLOCKCHAIN-OF-TIMER-EXPIRES-NEXT
 (62 8 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (33 1 (:REWRITE ALEOBFT-DYNAMIC::GET-VALIDATOR-STATE-OF-UPDATE-VALIDATOR-STATE-DIFF))
 (30 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (18 18 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (16 4 (:REWRITE SET::IN-TAIL))
 (12 12 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (12 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (8 8 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (6 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (6 6 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 (6 2 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (6 2 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::GET-NETWORK-STATE-OF-TIMER-EXPIRES-NEXT)
(ALEOBFT-DYNAMIC::TIMER-EXPIRES-NEXT-OF-ADDRESS-FIX-VAL
 (10 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (5 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (3 3 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (2 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::TIMER-EXPIRES-NEXT-ADDRESS-EQUIV-CONGRUENCE-ON-VAL)
(ALEOBFT-DYNAMIC::TIMER-EXPIRES-NEXT-OF-SYSTEM-STATE-FIX-SYSTATE
 (3 1 (:REWRITE ALEOBFT-DYNAMIC::SYSTEM-STATE-FIX-WHEN-SYSTEM-STATEP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::SYSTEM-STATEP))
 )
(ALEOBFT-DYNAMIC::TIMER-EXPIRES-NEXT-SYSTEM-STATE-EQUIV-CONGRUENCE-ON-SYSTATE)
