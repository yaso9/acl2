(LEN-OF-RESIZE-LIST
 (50 4 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (33 17 (:LINEAR LEN-POSITIVE-WHEN-CONSP-LINEAR-CHEAP))
 (25 21 (:REWRITE DEFAULT-<-2))
 (23 22 (:REWRITE DEFAULT-+-2))
 (22 22 (:REWRITE DEFAULT-+-1))
 (22 21 (:REWRITE DEFAULT-<-1))
 (22 14 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (8 1 (:REWRITE LEN-OF-CDR))
 (6 6 (:REWRITE DEFAULT-CDR))
 (6 6 (:REWRITE DEFAULT-CAR))
 (4 4 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (4 4 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (1 1 (:REWRITE EQUAL-OF-LEN-AND-0))
 (1 1 (:REWRITE CONSP-OF-CDR-WHEN-LEN-KNOWN))
 )
(CONSP-OF-RESIZE-LIST
 (82 8 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (33 29 (:REWRITE DEFAULT-<-2))
 (29 29 (:REWRITE DEFAULT-<-1))
 (19 18 (:REWRITE DEFAULT-+-2))
 (18 18 (:REWRITE DEFAULT-+-1))
 (9 3 (:REWRITE FOLD-CONSTS-IN-+))
 (8 8 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (8 8 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (8 1 (:REWRITE LEN-OF-CDR))
 (6 6 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (6 6 (:REWRITE DEFAULT-CDR))
 (6 6 (:REWRITE DEFAULT-CAR))
 (1 1 (:REWRITE EQUAL-OF-LEN-AND-0))
 (1 1 (:REWRITE CONSP-OF-CDR-WHEN-LEN-KNOWN))
 )
(CAR-OF-RESIZE-LIST
 (68 7 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (59 31 (:REWRITE DEFAULT-CAR))
 (57 50 (:REWRITE DEFAULT-<-2))
 (50 50 (:REWRITE DEFAULT-<-1))
 (28 27 (:REWRITE DEFAULT-+-2))
 (27 27 (:REWRITE DEFAULT-+-1))
 (14 14 (:REWRITE DEFAULT-CDR))
 (12 6 (:REWRITE CONSP-OF-RESIZE-LIST))
 (12 4 (:REWRITE FOLD-CONSTS-IN-+))
 (11 11 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (7 7 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (7 7 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (6 2 (:DEFINITION POSP))
 (2 2 (:REWRITE CDR-CONS))
 )
(RESIZE-LIST-OF-0)
(CDR-SUB1-SUB1-INDUCT)
(RESIZE-LIST-WHEN-NOT-CONSP
 (40 4 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (20 20 (:TYPE-PRESCRIPTION LEN))
 (18 14 (:REWRITE DEFAULT-<-2))
 (14 14 (:REWRITE DEFAULT-<-1))
 (5 5 (:REWRITE DEFAULT-+-2))
 (5 5 (:REWRITE DEFAULT-+-1))
 (4 4 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (4 4 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (4 4 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (4 4 (:LINEAR LEN-POSITIVE-WHEN-CONSP-LINEAR-CHEAP))
 )
(NTH-OF-RESIZE-LIST-2
 (248 21 (:REWRITE CONSP-FROM-LEN-CHEAP))
 (92 74 (:REWRITE DEFAULT-<-2))
 (74 74 (:REWRITE DEFAULT-<-1))
 (64 52 (:REWRITE DEFAULT-+-2))
 (52 52 (:REWRITE DEFAULT-+-1))
 (31 18 (:REWRITE NTH-WHEN-<=-LEN-CHEAP))
 (21 21 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (15 15 (:TYPE-PRESCRIPTION RESIZE-LIST))
 (15 15 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (7 7 (:REWRITE CONSP-OF-CDR-WHEN-LEN-KNOWN))
 (6 6 (:REWRITE DEFAULT-CAR))
 (5 5 (:REWRITE ZP-OPEN))
 (5 5 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (5 5 (:REWRITE NTH-OF-CONS-CONSTANT-VERSION))
 )
(RESIZE-LIST-OF-UPDATE-NTH
 (506 118 (:REWRITE DEFAULT-CDR))
 (366 88 (:REWRITE DEFAULT-CAR))
 (241 56 (:REWRITE RESIZE-LIST-WHEN-NOT-CONSP))
 (219 205 (:REWRITE DEFAULT-+-2))
 (205 205 (:REWRITE DEFAULT-+-1))
 (175 171 (:REWRITE LEN-WHEN-NOT-CONSP-CHEAP))
 (167 167 (:REWRITE CONSP-WHEN-LEN-EQUAL-CONSTANT))
 (165 144 (:REWRITE DEFAULT-<-2))
 (145 144 (:REWRITE DEFAULT-<-1))
 (98 31 (:REWRITE FOLD-CONSTS-IN-+))
 (53 20 (:REWRITE ZP-OPEN))
 (33 12 (:REWRITE CONSP-OF-RESIZE-LIST))
 (25 2 (:REWRITE CAR-OF-RESIZE-LIST))
 (21 7 (:DEFINITION POSP))
 (11 11 (:REWRITE CONSP-WHEN-LEN-GREATER))
 (10 10 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (7 7 (:REWRITE CONSP-OF-CDR-WHEN-LEN-KNOWN))
 (3 1 (:DEFINITION NATP))
 (1 1 (:TYPE-PRESCRIPTION NATP))
 )