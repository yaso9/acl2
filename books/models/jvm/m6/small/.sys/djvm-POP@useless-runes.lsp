(CONSISTENT-STATE-OP-CODE-POP-IMPLIES-PC-IN-RANGE)
(CURRENT-METHOD-NORMALIZE)
(BCV-SIMPLE-EXECUTE-STEP-INST-POP-FACT-1
 (16 9 (:REWRITE DEFAULT-+-2))
 (9 9 (:REWRITE DEFAULT-+-1))
 (8 8 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (4 3 (:REWRITE DEFAULT-<-2))
 (3 3 (:REWRITE DEFAULT-<-1))
 (1 1 (:REWRITE DEFAULT-CAR))
 )
(BCV-SIMPLE-EXECUTE-STEP-INST-POP-FACT-2
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE DEFAULT-<-1))
 (2 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:REWRITE DEFAULT-+-1))
 (2 2 (:DEFINITION ADVANCE-PC))
 )
(|Subgoal 4|
 (42 6 (:DEFINITION ASSOC-EQUAL))
 (31 28 (:REWRITE DEFAULT-CAR))
 (12 12 (:TYPE-PRESCRIPTION COLLECT-WITNESS-BCV-METHOD))
 (12 12 (:REWRITE DEFAULT-CDR))
 (3 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:REWRITE DEFAULT-+-1))
 (2 2 (:REWRITE ALL-NEXT-STATE-SAFE-IMPLIES-PC-EQUAL))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(BCV-SIMPLE-EXECUTE-STEP-INST-POP-FACT-3
 (8 5 (:REWRITE DEFAULT-+-2))
 (5 5 (:REWRITE DEFAULT-+-1))
 (4 4 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (4 3 (:REWRITE DEFAULT-<-2))
 (3 3 (:REWRITE DEFAULT-<-1))
 (1 1 (:REWRITE DEFAULT-CAR))
 )
(|Subgoal 3|
 (42 6 (:DEFINITION ASSOC-EQUAL))
 (31 28 (:REWRITE DEFAULT-CAR))
 (12 12 (:TYPE-PRESCRIPTION COLLECT-WITNESS-BCV-METHOD))
 (12 12 (:REWRITE DEFAULT-CDR))
 (2 1 (:REWRITE DEFAULT-+-2))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (1 1 (:REWRITE DEFAULT-+-1))
 (1 1 (:REWRITE ALL-NEXT-STATE-SAFE-IMPLIES-PC-EQUAL))
 (1 1 (:REWRITE ALL-NEXT-STATE-SAFE-IMPLIES-MAX-STACK-EQUAL))
 )
(BCV-SIMPLE-EXECUTE-STEP-INST-POP-FACT-4
 (8 5 (:REWRITE DEFAULT-+-2))
 (5 5 (:REWRITE DEFAULT-+-1))
 (4 4 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (4 3 (:REWRITE DEFAULT-<-2))
 (3 3 (:REWRITE DEFAULT-<-1))
 (1 1 (:REWRITE DEFAULT-CAR))
 )
(|Subgoal 2|
 (49 7 (:DEFINITION ASSOC-EQUAL))
 (35 32 (:REWRITE DEFAULT-CAR))
 (14 14 (:TYPE-PRESCRIPTION COLLECT-WITNESS-BCV-METHOD))
 (14 14 (:REWRITE DEFAULT-CDR))
 (2 1 (:REWRITE DEFAULT-+-2))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (1 1 (:REWRITE DEFAULT-+-1))
 (1 1 (:REWRITE ALL-NEXT-STATE-SAFE-IMPLIES-PC-EQUAL))
 )
(BCV-SIMPLE-CHECK-STEP-PRE-POP
 (6 2 (:LINEAR CONSISTENT-STATE-LEN-OPSTACK-IN-LIMIT))
 (4 4 (:TYPE-PRESCRIPTION CONSISTENT-STATE))
 (4 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE DEFAULT-+-1))
 )
(BCV-SIMPLE-CHECK-STEP-PRE-POP-SPECIFIC
 (6 6 (:TYPE-PRESCRIPTION CONSISTENT-STATE))
 (6 2 (:LINEAR CONSISTENT-STATE-LEN-OPSTACK-IN-LIMIT))
 (4 2 (:REWRITE DEFAULT-+-2))
 (3 1 (:REWRITE CONSISTENT-STATE-BCV-SIMPLE-CHECK-STEP-PRE-IF-PC-IN-RANGE))
 (2 2 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE DEFAULT-+-1))
 )
(BCV-SIMPLE-EXECUTE-STEP-INST-POP-FACT-5
 (10 7 (:REWRITE DEFAULT-+-2))
 (7 7 (:REWRITE DEFAULT-+-1))
 (5 4 (:REWRITE DEFAULT-<-2))
 (4 4 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (4 4 (:REWRITE DEFAULT-<-1))
 (1 1 (:REWRITE DEFAULT-CAR))
 )
(|Subgoal 1|
 (63 9 (:DEFINITION ASSOC-EQUAL))
 (47 44 (:REWRITE DEFAULT-CAR))
 (23 23 (:REWRITE DEFAULT-CDR))
 (18 18 (:TYPE-PRESCRIPTION COLLECT-WITNESS-BCV-METHOD))
 (18 10 (:REWRITE DEFAULT-+-2))
 (10 10 (:REWRITE DEFAULT-+-1))
 (3 3 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (2 1 (:REWRITE DEFAULT-<-2))
 (1 1 (:REWRITE DEFAULT-<-1))
 (1 1 (:REWRITE ALL-NEXT-STATE-SAFE-IMPLIES-PC-EQUAL))
 )
(BCV-SIMPLE-EXECUTE-STEP-INST-METHOD-NAME-FACT
 (8 5 (:REWRITE DEFAULT-+-2))
 (5 5 (:REWRITE DEFAULT-+-1))
 (4 4 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (4 3 (:REWRITE DEFAULT-<-2))
 (3 3 (:REWRITE DEFAULT-<-1))
 (1 1 (:REWRITE DEFAULT-CAR))
 )
(INVOKE-PRODUCE-CONSP
 (1 1 (:REWRITE DEFAULT-<-2))
 (1 1 (:REWRITE DEFAULT-<-1))
 (1 1 (:REWRITE DEFAULT-+-2))
 (1 1 (:REWRITE DEFAULT-+-1))
 (1 1 (:DEFINITION ADVANCE-PC))
 )
(|Subgoal 0|
 (49 7 (:DEFINITION ASSOC-EQUAL))
 (38 35 (:REWRITE DEFAULT-CAR))
 (18 18 (:TYPE-PRESCRIPTION COLLECT-WITNESS-BCV-METHOD))
 (17 2 (:REWRITE WFF-METHOD-TABLE-IMPLIES-G-METHOD-NAME-NORMALIZE))
 (16 16 (:REWRITE DEFAULT-CDR))
 (13 1 (:DEFINITION WFF-METHOD-TABLE))
 (2 2 (:REWRITE SIG-FRAME-COMPATIBLE-EXTRACT-SIG-FRAME-METHOD-NAME-2))
 (2 1 (:REWRITE DEFAULT-+-2))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (1 1 (:REWRITE INVOKE-PRODUCE-CONSP))
 (1 1 (:REWRITE DEFAULT-+-1))
 (1 1 (:REWRITE ALL-NEXT-STATE-SAFE-IMPLIES-PC-EQUAL))
 )
(LEN-CONSP)
(CONSISTENT-STATE-PRESERVED-BY-POP-LEMMA
 (569 73 (:REWRITE DEFAULT-CAR))
 (179 5 (:DEFINITION ASSOC-EQUAL))
 (163 82 (:REWRITE DEFAULT-+-2))
 (115 60 (:REWRITE DEFAULT-<-2))
 (82 82 (:REWRITE DEFAULT-+-1))
 (60 60 (:REWRITE DEFAULT-<-1))
 (8 8 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (6 2 (:REWRITE CONSISTENT-STATE-PC-IN-RANGE-B))
 (6 2 (:REWRITE CONSISTENT-STATE-OP-CODE-POP-IMPLIES-PC-IN-RANGE))
 (4 4 (:TYPE-PRESCRIPTION COLLECT-WITNESS-BCV-METHOD))
 )
(CONSISTENT-STATE-STEP-IMPLIES-CONSISTENT-STATE-DJVM-EXECUTE-POP)
(CONSISTENT-STATE-PRESERVED-BY-DJVM-POP
 (85 10 (:REWRITE DEFAULT-CAR))
 (63 9 (:DEFINITION LEN))
 (22 11 (:REWRITE DEFAULT-+-2))
 (13 11 (:REWRITE DEFAULT-CDR))
 (11 11 (:REWRITE DEFAULT-+-1))
 (10 5 (:REWRITE DEFAULT-<-2))
 (6 2 (:REWRITE CONSISTENT-STATE-PC-IN-RANGE-B))
 (6 2 (:REWRITE CONSISTENT-STATE-OP-CODE-POP-IMPLIES-PC-IN-RANGE))
 (5 5 (:REWRITE DEFAULT-<-1))
 (2 2 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
