(HTCLIENT::STATE-P1-OF-READ-ACL2-ORACLE
 (6 3 (:DEFINITION NTH))
 (6 2 (:REWRITE STATE-P-IMPLIES-AND-FORWARD-TO-STATE-P1))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 1 (:DEFINITION STATE-P))
 (2 2 (:TYPE-PRESCRIPTION STATE-P))
 (1 1 (:REWRITE DEFAULT-CAR))
 )
(HTCLIENT::POST)
(HTCLIENT::STRINGP-OF-POST.VAL
 (1 1 (:REWRITE STR-FIX-WHEN-STRINGP))
 (1 1 (:REWRITE STR-FIX-DEFAULT))
 )
(HTCLIENT::STATE-P1-OF-POST.STATE
 (12 4 (:REWRITE STATE-P-IMPLIES-AND-FORWARD-TO-STATE-P1))
 (8 2 (:DEFINITION STATE-P))
 (4 4 (:TYPE-PRESCRIPTION STATE-P))
 (1 1 (:REWRITE STR-FIX-WHEN-STRINGP))
 (1 1 (:REWRITE STR-FIX-DEFAULT))
 )