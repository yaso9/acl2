(BOUNDED-INTEGER-ALISTP-OF-APPEND
 (225 174 (:REWRITE DEFAULT-CAR))
 (144 72 (:TYPE-PRESCRIPTION TRUE-LISTP-APPEND))
 (83 71 (:REWRITE DEFAULT-CDR))
 (74 74 (:REWRITE DEFAULT-<-2))
 (74 74 (:REWRITE DEFAULT-<-1))
 (72 72 (:TYPE-PRESCRIPTION BINARY-APPEND))
 )
(BOUNDED-INTEGER-ALISTP-OF-REVAPPEND
 (68 64 (:REWRITE DEFAULT-CAR))
 (36 36 (:REWRITE DEFAULT-<-2))
 (36 36 (:REWRITE DEFAULT-<-1))
 (34 30 (:REWRITE DEFAULT-CDR))
 )
(INTEGERP-OF-CAR-OF-ASSOC-EQUAL-WHEN-BOUNDED-INTEGER-ALISTP
 (138 138 (:REWRITE DEFAULT-CAR))
 (20 20 (:REWRITE DEFAULT-CDR))
 (20 20 (:REWRITE DEFAULT-<-2))
 (20 20 (:REWRITE DEFAULT-<-1))
 )
(BOUND-OF-CAR-OF-ASSOC-EQUAL-WHEN-BOUNDED-INTEGER-ALISTP
 (189 189 (:REWRITE DEFAULT-CAR))
 (63 63 (:REWRITE DEFAULT-<-2))
 (35 35 (:REWRITE DEFAULT-CDR))
 (12 12 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(NOT-ASSOC-EQUAL-WHEN-BOUNDED-INTEGER-ALISTP-OUT-OF-BOUNDS
 (35 35 (:REWRITE DEFAULT-CAR))
 (29 24 (:REWRITE DEFAULT-<-2))
 (24 24 (:REWRITE DEFAULT-<-1))
 (9 9 (:REWRITE DEFAULT-CDR))
 (5 5 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(BOUND2-OF-CAR-OF-ASSOC-EQUAL-WHEN-BOUNDED-INTEGER-ALISTP
 (89 89 (:REWRITE DEFAULT-CAR))
 (75 5 (:REWRITE BOUND-OF-CAR-OF-ASSOC-EQUAL-WHEN-BOUNDED-INTEGER-ALISTP))
 (35 30 (:REWRITE DEFAULT-<-1))
 (30 30 (:REWRITE DEFAULT-<-2))
 (13 13 (:REWRITE DEFAULT-CDR))
 (5 5 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(BOUNDED-INTEGER-ALISTP-OF-CONS
 (20 20 (:REWRITE DEFAULT-CAR))
 (13 13 (:REWRITE DEFAULT-<-2))
 (13 13 (:REWRITE DEFAULT-<-1))
 (8 8 (:REWRITE DEFAULT-CDR))
 )
(BOUNDED-INTEGER-ALISTP-OF-NIL)
(ASSOC-EQUAL-FORWARD-WHEN-BOUNDED-INTEGER-ALISTP
 (44 40 (:REWRITE DEFAULT-<-2))
 (42 40 (:REWRITE DEFAULT-<-1))
 (19 19 (:REWRITE DEFAULT-CDR))
 (6 6 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )