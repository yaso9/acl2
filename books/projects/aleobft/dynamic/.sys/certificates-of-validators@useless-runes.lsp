(ALEOBFT-DYNAMIC::OWNED-CERTIFICATES)
(ALEOBFT-DYNAMIC::CERTIFICATE-SETP-OF-OWNED-CERTIFICATES
 (47 10 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-MESSAGE-SETP))
 (43 10 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS+POS-SETP))
 (34 1 (:REWRITE ALEOBFT-DYNAMIC::MESSAGE-SETP-OF-UNION))
 (28 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS+POS-SETP-OF-UNION))
 (23 10 (:REWRITE SET::SFIX-WHEN-EMPTYP))
 (22 22 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (12 12 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::MESSAGE-SETP))
 (12 12 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SETP))
 (9 2 (:REWRITE SET::UNION-EMPTYP-Y))
 (8 2 (:REWRITE SET::UNION-EMPTYP))
 (6 2 (:REWRITE SET::UNION-EMPTYP-X))
 )
(ALEOBFT-DYNAMIC::MESSAGE-CERTIFICATE-IN-OWNED-CERTIFICATES
 (32 8 (:REWRITE SET::IN-TAIL))
 (26 26 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (12 4 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (12 4 (:REWRITE SET::NEVER-IN-EMPTY))
 (9 2 (:REWRITE SET::UNION-EMPTYP-Y))
 (8 2 (:REWRITE ALEOBFT-DYNAMIC::MESSAGE-FIX-WHEN-MESSAGEP))
 (6 2 (:REWRITE SET::UNION-EMPTYP-X))
 (4 4 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::MESSAGEP))
 (4 1 (:REWRITE SET::UNION-EMPTYP))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::MESSAGEP-WHEN-IN-MESSAGE-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::OWNED-CERTIFICATES-OF-ADDRESS-FIX-VAL
 (11 4 (:REWRITE SET::UNION-EMPTYP-Y))
 (10 10 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (10 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (8 4 (:REWRITE SET::UNION-EMPTYP-X))
 (5 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (4 1 (:REWRITE SET::UNION-EMPTYP))
 (3 3 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (2 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::OWNED-CERTIFICATES-ADDRESS-EQUIV-CONGRUENCE-ON-VAL)
(ALEOBFT-DYNAMIC::OWNED-CERTIFICATES-OF-SYSTEM-STATE-FIX-SYSTATE
 (11 4 (:REWRITE SET::UNION-EMPTYP-Y))
 (10 10 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (8 4 (:REWRITE SET::UNION-EMPTYP-X))
 (4 1 (:REWRITE SET::UNION-EMPTYP))
 (3 1 (:REWRITE ALEOBFT-DYNAMIC::SYSTEM-STATE-FIX-WHEN-SYSTEM-STATEP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::SYSTEM-STATEP))
 )
(ALEOBFT-DYNAMIC::OWNED-CERTIFICATES-SYSTEM-STATE-EQUIV-CONGRUENCE-ON-SYSTATE)
(ALEOBFT-DYNAMIC::OWNED-CERTIFICATES-WHEN-INIT
 (8 2 (:REWRITE SET::IN-TAIL))
 (4 2 (:REWRITE SET::UNION-EMPTYP-Y))
 (4 2 (:REWRITE SET::SFIX-WHEN-EMPTYP))
 (4 2 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-MESSAGE-SETP))
 (4 2 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS+POS-SETP))
 (3 3 (:REWRITE-QUOTED-CONSTANT ALEOBFT-DYNAMIC::CERTIFICATE-SET-FIX-UNDER-CERTIFICATE-SET-EQUIV))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (3 1 (:REWRITE SET::EMPTYP-SUBSET-2))
 (3 1 (:REWRITE SET::EMPTYP-SUBSET))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::MESSAGE-SETP))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SETP))
 (1 1 (:REWRITE-QUOTED-CONSTANT POS-FIX-UNDER-POS-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT NFIX-UNDER-NAT-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT ALEOBFT-DYNAMIC::BLOCK-LIST-FIX-UNDER-BLOCK-LIST-EQUIV))
 (1 1 (:REWRITE-QUOTED-CONSTANT ALEOBFT-DYNAMIC::ADDRESS+POS-SET-FIX-UNDER-ADDRESS+POS-SET-EQUIV))
 (1 1 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 )
(ALEOBFT-DYNAMIC::OWNED-CERTIFICATES-OF-CREATE-CERTIFICATE-NEXT
 (111 7 (:REWRITE SET::INSERT-IDENTITY))
 (110 29 (:REWRITE SET::IN-TAIL))
 (93 6 (:REWRITE SET::UNION-IN))
 (92 76 (:TYPE-PRESCRIPTION SET::INSERT))
 (44 12 (:REWRITE SET::UNION-EMPTYP))
 (39 13 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (39 7 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATE-FIX-WHEN-CERTIFICATEP))
 (33 7 (:REWRITE SET::INSERT-WHEN-EMPTYP))
 (32 14 (:REWRITE SET::UNION-EMPTYP-X))
 (20 4 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-CERTIFICATE-OPTIONP))
 (13 5 (:REWRITE SET::NEVER-IN-EMPTY))
 (12 12 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATEP))
 (8 8 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP))
 (8 4 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP-WHEN-CERTIFICATEP))
 (8 1 (:REWRITE SET::DELETE-NONMEMBER-CANCEL))
 (6 2 (:REWRITE SET::SFIX-WHEN-EMPTYP))
 (6 2 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-MESSAGE-SETP))
 (6 2 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS+POS-SETP))
 (5 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (4 4 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SETP))
 (4 4 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-IN-CERTIFICATE-SETP-BINDS-FREE-X))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (2 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::OWNED-CERTIFICATES-OF-RECEIVE-CERTIFICATE-NEXT
 (1516 156 (:REWRITE SET::UNION-WITH-SUBSET-RIGHT))
 (1516 156 (:REWRITE SET::UNION-WITH-SUBSET-LEFT))
 (888 372 (:REWRITE SET::EMPTYP-SUBSET))
 (878 372 (:REWRITE SET::EMPTYP-SUBSET-2))
 (709 134 (:REWRITE SET::IN-TAIL))
 (676 100 (:LINEAR SET::PROPER-SUBSET-CARDINALITY))
 (576 156 (:REWRITE SET::UNION-EMPTYP-Y))
 (364 364 (:REWRITE SET::PICK-A-POINT-SUBSET-STRATEGY))
 (364 364 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (340 156 (:REWRITE SET::UNION-EMPTYP-X))
 (340 20 (:REWRITE SET::DELETE-NONMEMBER-CANCEL))
 (328 86 (:REWRITE SET::UNION-EMPTYP))
 (206 158 (:REWRITE SET::SUBSET-IN-2))
 (189 63 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (173 59 (:REWRITE SET::NEVER-IN-EMPTY))
 (160 43 (:REWRITE ALEOBFT-DYNAMIC::MESSAGE-FIX-WHEN-MESSAGEP))
 (78 78 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::MESSAGEP))
 (77 77 (:REWRITE ALEOBFT-DYNAMIC::NOT-EMPTYP-OF-COMMITTEE-MEMBERS))
 (63 63 (:REWRITE SET::SUBSET-MEMBERSHIP-TAIL))
 (39 39 (:REWRITE ALEOBFT-DYNAMIC::MESSAGEP-WHEN-IN-MESSAGE-SETP-BINDS-FREE-X))
 (36 8 (:REWRITE SET::INSERT-WHEN-EMPTYP))
 (36 8 (:REWRITE SET::DELETE-PRESERVES-EMPTYP))
 (24 24 (:REWRITE-QUOTED-CONSTANT POS-FIX-UNDER-POS-EQUIV))
 (24 12 (:REWRITE SETP-WHEN-POS-SETP))
 (24 12 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-MESSAGE-SETP))
 (24 12 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-CERTIFICATE-SETP))
 (24 12 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS-SETP))
 (24 12 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS+POS-SETP))
 (16 8 (:REWRITE SET::NONEMPTY-MEANS-SET))
 (12 12 (:TYPE-PRESCRIPTION POS-SETP))
 (12 12 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-SETP))
 (12 12 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SETP))
 (10 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (8 8 (:REWRITE SET::IN-SET))
 (6 6 (:REWRITE SET::INSERT-NEVER-EMPTY))
 (4 4 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (4 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::OWNED-CERTIFICATES-OF-STORE-CERTIFICATE-NEXT
 (986 986 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (706 72 (:REWRITE SET::UNION-WITH-SUBSET-LEFT))
 (704 72 (:REWRITE SET::UNION-WITH-SUBSET-RIGHT))
 (593 98 (:REWRITE SET::IN-TAIL))
 (489 197 (:REWRITE SET::EMPTYP-SUBSET-2))
 (485 197 (:REWRITE SET::EMPTYP-SUBSET))
 (380 8 (:REWRITE SET::INSERT-IDENTITY))
 (324 18 (:REWRITE SET::DELETE-NONMEMBER-CANCEL))
 (314 114 (:REWRITE SET::SUBSET-IN-2))
 (274 72 (:REWRITE SET::UNION-EMPTYP-Y))
 (251 35 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATE-FIX-WHEN-CERTIFICATEP))
 (240 64 (:REWRITE SET::UNION-EMPTYP))
 (193 193 (:REWRITE SET::PICK-A-POINT-SUBSET-STRATEGY))
 (193 193 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (172 72 (:REWRITE SET::UNION-EMPTYP-X))
 (123 41 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (116 17 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (111 41 (:REWRITE SET::NEVER-IN-EMPTY))
 (55 11 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (41 41 (:REWRITE SET::SUBSET-MEMBERSHIP-TAIL))
 (36 8 (:REWRITE SET::INSERT-WHEN-EMPTYP))
 (36 8 (:REWRITE SET::DELETE-PRESERVES-EMPTYP))
 (33 33 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (22 22 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (22 11 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (12 6 (:REWRITE SETP-WHEN-POS-SETP))
 (12 6 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-MESSAGE-SETP))
 (12 6 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-CERTIFICATE-SETP))
 (12 6 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS-SETP))
 (12 6 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS+POS-SETP))
 (11 11 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (11 11 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 (6 6 (:TYPE-PRESCRIPTION POS-SETP))
 (6 6 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-SETP))
 (6 6 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SETP))
 (6 6 (:REWRITE SET::INSERT-NEVER-EMPTY))
 (4 2 (:REWRITE SET::NONEMPTY-MEANS-SET))
 (2 2 (:REWRITE SET::IN-SET))
 )
(ALEOBFT-DYNAMIC::OWNED-CERTIFICATES-OF-ADVANCE-ROUND-NEXT
 (14 14 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (11 4 (:REWRITE SET::UNION-EMPTYP-Y))
 (8 4 (:REWRITE SET::UNION-EMPTYP-X))
 (8 2 (:REWRITE SET::IN-TAIL))
 (4 1 (:REWRITE SET::UNION-EMPTYP))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::OWNED-CERTIFICATES-OF-COMMIT-ANCHORS-NEXT
 (14 14 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (11 4 (:REWRITE SET::UNION-EMPTYP-Y))
 (8 4 (:REWRITE SET::UNION-EMPTYP-X))
 (8 2 (:REWRITE SET::IN-TAIL))
 (4 1 (:REWRITE SET::UNION-EMPTYP))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::OWNED-CERTIFICATES-OF-TIMER-EXPIRES-NEXT
 (14 14 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (11 4 (:REWRITE SET::UNION-EMPTYP-Y))
 (8 4 (:REWRITE SET::UNION-EMPTYP-X))
 (8 2 (:REWRITE SET::IN-TAIL))
 (4 1 (:REWRITE SET::UNION-EMPTYP))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::SIGNED-CERTIFICATES)
(ALEOBFT-DYNAMIC::CERTIFICATE-SETP-OF-SIGNED-CERTIFICATES)
(ALEOBFT-DYNAMIC::SIGNED-CERTIFICATES-OF-ADDRESS-FIX-VAL)
(ALEOBFT-DYNAMIC::SIGNED-CERTIFICATES-ADDRESS-EQUIV-CONGRUENCE-ON-VAL)
(ALEOBFT-DYNAMIC::SIGNED-CERTIFICATES-OF-SYSTEM-STATE-FIX-SYSTATE)
(ALEOBFT-DYNAMIC::SIGNED-CERTIFICATES-SYSTEM-STATE-EQUIV-CONGRUENCE-ON-SYSTATE)
(ALEOBFT-DYNAMIC::SIGNED-CERTIFICATES-WHEN-INIT)
(ALEOBFT-DYNAMIC::SIGNED-CERTIFICATES-OF-CREATE-CERTIFICATE-NEXT
 (23 8 (:REWRITE SET::IN-TAIL))
 (18 2 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATE-FIX-WHEN-CERTIFICATEP))
 (16 3 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-CERTIFICATE-OPTIONP))
 (15 3 (:REWRITE SET::INSERT-IDENTITY))
 (14 14 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (9 3 (:REWRITE SET::INSERT-WHEN-EMPTYP))
 (7 3 (:REWRITE SET::NEVER-IN-EMPTY))
 (7 3 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP-WHEN-CERTIFICATEP))
 (6 6 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP))
 (6 2 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (6 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (3 3 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-IN-CERTIFICATE-SETP-BINDS-FREE-X))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (2 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 )
(ALEOBFT-DYNAMIC::SIGNED-CERTIFICATES-OF-RECEIVE-CERTIFICATE-NEXT
 (8 2 (:REWRITE SET::IN-TAIL))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::SIGNED-CERTIFICATES-OF-STORE-CERTIFICATE-NEXT
 (8 2 (:REWRITE SET::IN-TAIL))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::SIGNED-CERTIFICATES-OF-ADVANCE-ROUND-NEXT
 (8 2 (:REWRITE SET::IN-TAIL))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::SIGNED-CERTIFICATES-OF-COMMIT-ANCHORS-NEXT
 (8 2 (:REWRITE SET::IN-TAIL))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::SIGNED-CERTIFICATES-OF-TIMER-EXPIRES-NEXT
 (8 2 (:REWRITE SET::IN-TAIL))
 (4 4 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::ACCEPTED-CERTIFICATES)
(ALEOBFT-DYNAMIC::CERTIFICATE-SETP-OF-ACCEPTED-CERTIFICATES
 (8 8 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (6 2 (:REWRITE SET::SFIX-WHEN-EMPTYP))
 (6 2 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-MESSAGE-SETP))
 (6 2 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS+POS-SETP))
 (4 4 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::MESSAGE-SETP))
 (4 4 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SETP))
 (3 1 (:REWRITE SET::UNION-EMPTYP-Y))
 (3 1 (:REWRITE SET::UNION-EMPTYP-X))
 )
(ALEOBFT-DYNAMIC::IN-OWNED-CERTIFICATES-WHEN-IN-ACCEPTED-CERTIFICATES
 (44 44 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (40 10 (:REWRITE SET::IN-TAIL))
 (21 5 (:REWRITE SET::UNION-EMPTYP-Y))
 (15 5 (:REWRITE SET::UNION-EMPTYP-X))
 (15 5 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (15 5 (:REWRITE SET::NEVER-IN-EMPTY))
 (8 2 (:REWRITE SET::UNION-EMPTYP))
 )
(ALEOBFT-DYNAMIC::ACCEPTED-CERTIFICATES-OF-ADDRESS-FIX-VAL)
(ALEOBFT-DYNAMIC::ACCEPTED-CERTIFICATES-ADDRESS-EQUIV-CONGRUENCE-ON-VAL)
(ALEOBFT-DYNAMIC::ACCEPTED-CERTIFICATES-OF-SYSTEM-STATE-FIX-SYSTATE)
(ALEOBFT-DYNAMIC::ACCEPTED-CERTIFICATES-SYSTEM-STATE-EQUIV-CONGRUENCE-ON-SYSTATE)
(ALEOBFT-DYNAMIC::ACCEPTED-CERTIFICATES-WHEN-INIT
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
(ALEOBFT-DYNAMIC::ACCEPTED-CERTIFICATES-OF-CREATE-CERTIFICATE-NEXT
 (36 20 (:TYPE-PRESCRIPTION SET::INSERT))
 (27 2 (:REWRITE SET::INSERT-IDENTITY))
 (26 26 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (26 7 (:REWRITE SET::IN-TAIL))
 (20 1 (:REWRITE SET::UNION-IN))
 (18 2 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATE-FIX-WHEN-CERTIFICATEP))
 (10 2 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-CERTIFICATE-OPTIONP))
 (9 3 (:REWRITE SET::UNION-EMPTYP-Y))
 (9 3 (:REWRITE SET::UNION-EMPTYP-X))
 (9 3 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (9 2 (:REWRITE SET::INSERT-WHEN-EMPTYP))
 (6 6 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATEP))
 (5 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (4 4 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP))
 (4 2 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP-WHEN-CERTIFICATEP))
 (4 1 (:REWRITE SET::UNION-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (2 2 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-IN-CERTIFICATE-SETP-BINDS-FREE-X))
 (2 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::ACCEPTED-CERTIFICATES-OF-RECEIVE-CERTIFICATE-NEXT
 (40 22 (:TYPE-PRESCRIPTION SET::INSERT))
 (27 2 (:REWRITE SET::INSERT-IDENTITY))
 (26 26 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (26 7 (:REWRITE SET::IN-TAIL))
 (20 1 (:REWRITE SET::UNION-IN))
 (9 3 (:REWRITE SET::UNION-EMPTYP-Y))
 (9 3 (:REWRITE SET::UNION-EMPTYP-X))
 (9 3 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (9 2 (:REWRITE SET::INSERT-WHEN-EMPTYP))
 (5 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (4 1 (:REWRITE SET::UNION-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 (2 2 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (2 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (1 1 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 )
(ALEOBFT-DYNAMIC::ACCEPTED-CERTIFICATES-OF-STORE-CERTIFICATE-NEXT
 (390 390 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (240 24 (:REWRITE SET::UNION-WITH-SUBSET-RIGHT))
 (240 24 (:REWRITE SET::UNION-WITH-SUBSET-LEFT))
 (221 38 (:REWRITE SET::IN-TAIL))
 (201 81 (:REWRITE SET::EMPTYP-SUBSET-2))
 (197 81 (:REWRITE SET::EMPTYP-SUBSET))
 (141 21 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATE-FIX-WHEN-CERTIFICATEP))
 (132 10 (:REWRITE SET::DELETE-NONMEMBER-CANCEL))
 (106 6 (:REWRITE SET::INSERT-IDENTITY))
 (94 46 (:REWRITE SET::SUBSET-IN-2))
 (80 24 (:REWRITE SET::UNION-EMPTYP-X))
 (77 77 (:REWRITE SET::PICK-A-POINT-SUBSET-STRATEGY))
 (77 77 (:REWRITE SET::PICK-A-POINT-SUBSET-CONSTRAINT-HELPER))
 (76 13 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-FIX-WHEN-ADDRESSP))
 (75 15 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-CERTIFICATE-OPTIONP))
 (64 24 (:REWRITE SET::UNION-EMPTYP-Y))
 (51 17 (:REWRITE SET::NEVER-IN-EMPTY))
 (48 12 (:REWRITE SET::UNION-EMPTYP))
 (45 45 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATEP))
 (45 15 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (35 7 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-ADDRESS-OPTIONP))
 (30 30 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP))
 (30 15 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATE-OPTIONP-WHEN-CERTIFICATEP))
 (21 21 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESSP))
 (18 6 (:REWRITE SET::INSERT-WHEN-EMPTYP))
 (15 15 (:REWRITE SET::SUBSET-MEMBERSHIP-TAIL))
 (15 15 (:REWRITE ALEOBFT-DYNAMIC::CERTIFICATEP-WHEN-IN-CERTIFICATE-SETP-BINDS-FREE-X))
 (14 14 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-OPTIONP))
 (14 7 (:REWRITE ALEOBFT-DYNAMIC::ADDRESS-OPTIONP-WHEN-ADDRESSP))
 (12 6 (:REWRITE SETP-WHEN-POS-SETP))
 (12 6 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-MESSAGE-SETP))
 (12 6 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-CERTIFICATE-SETP))
 (12 6 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS-SETP))
 (12 6 (:REWRITE ALEOBFT-DYNAMIC::SETP-WHEN-ADDRESS+POS-SETP))
 (12 4 (:REWRITE SET::DELETE-PRESERVES-EMPTYP))
 (7 7 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-VALIDATORS-STATEP-BINDS-FREE-X))
 (7 7 (:REWRITE ALEOBFT-DYNAMIC::ADDRESSP-WHEN-IN-ADDRESS-SETP-BINDS-FREE-X))
 (6 6 (:TYPE-PRESCRIPTION POS-SETP))
 (6 6 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::MESSAGE-SETP))
 (6 6 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::CERTIFICATE-SETP))
 (6 6 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS-SETP))
 (6 6 (:TYPE-PRESCRIPTION ALEOBFT-DYNAMIC::ADDRESS+POS-SETP))
 (4 4 (:REWRITE SET::INSERT-NEVER-EMPTY))
 (4 2 (:REWRITE SET::NONEMPTY-MEANS-SET))
 (2 2 (:REWRITE SET::IN-SET))
 )
(ALEOBFT-DYNAMIC::ACCEPTED-CERTIFICATES-OF-ADVANCE-ROUND-NEXT
 (8 8 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (8 2 (:REWRITE SET::IN-TAIL))
 (4 2 (:REWRITE SET::UNION-EMPTYP-Y))
 (4 2 (:REWRITE SET::UNION-EMPTYP-X))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::ACCEPTED-CERTIFICATES-OF-COMMIT-ANCHORS-NEXT
 (8 8 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (8 2 (:REWRITE SET::IN-TAIL))
 (4 2 (:REWRITE SET::UNION-EMPTYP-Y))
 (4 2 (:REWRITE SET::UNION-EMPTYP-X))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 )
(ALEOBFT-DYNAMIC::ACCEPTED-CERTIFICATES-OF-TIMER-EXPIRES-NEXT
 (8 8 (:TYPE-PRESCRIPTION SET::EMPTYP-TYPE))
 (8 2 (:REWRITE SET::IN-TAIL))
 (4 2 (:REWRITE SET::UNION-EMPTYP-Y))
 (4 2 (:REWRITE SET::UNION-EMPTYP-X))
 (3 1 (:REWRITE SET::TAIL-WHEN-EMPTYP))
 (3 1 (:REWRITE SET::NEVER-IN-EMPTY))
 )
