(C::EXEC-EXPR-CALL-OR-ASG-WHEN-CALL
 (10 2 (:REWRITE C::VALUE-OPTIONP-WHEN-VALUEP))
 (6 6 (:REWRITE DEFAULT-+-2))
 (6 6 (:REWRITE DEFAULT-+-1))
 (5 1 (:REWRITE C::VALUEP-WHEN-VALUE-OPTIONP))
 (3 3 (:TYPE-PRESCRIPTION C::VALUEP))
 (2 2 (:REWRITE C::VALUEP-WHEN-MEMBER-EQUAL-OF-VALUE-LISTP))
 (1 1 (:REWRITE ZP-OPEN))
 )
(C::EXEC-EXPR-CALL-OR-ASG-WHEN-ASG
 (8 2 (:REWRITE C::COMPUSTATEP-WHEN-COMPUSTATE-OPTIONP))
 (5 1 (:REWRITE C::COMPUSTATE-OPTIONP-WHEN-COMPUSTATEP))
 (3 3 (:TYPE-PRESCRIPTION C::COMPUSTATE-OPTIONP))
 (3 3 (:REWRITE DEFAULT-+-2))
 (3 3 (:REWRITE DEFAULT-+-1))
 (1 1 (:REWRITE ZP-OPEN))
 )