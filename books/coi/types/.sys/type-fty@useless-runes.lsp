(TYPES::PAIRLIST-META)
(TYPES::SAFE-PACKAGE-WITNESS)
(TYPES::T-IMPLIES-SYMBOLP-SAFE-PACKAGE-WITNESS)
(TYPES::SAFE-PACKAGE-WITNESS)
(TYPES::SYMBOL-EQUIV-1-IMPLIES-EQUAL-SAFE-PACKAGE-WITNESS
 (4 4 (:REWRITE SYMBOL-FIX-WHEN-SYMBOLP))
 (4 2 (:REWRITE DEFAULT-SYMBOL-PACKAGE-NAME))
 )
(TYPES::GOOD-ATOMP)
(TYPES::SAFE-PACKN-POS)
(TYPES::GOOD-ATOM-LISTP-SYMBOLP-IMPLIES-SYMBOLP-SAFE-PACKN-POS)
(TYPES::SAFE-PACKN-POS)
(TYPES::DEFAULT-NAME)
(TYPES::SYMBOLP-SYMBOLP-GOOD-ATOMP-SYMBOLP-IMPLIES-SYMBOLP-DEFAULT-NAME
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(TYPES::DEFAULT-NAME
 (18 18 (:REWRITE DEFAULT-CAR))
 (8 8 (:REWRITE DEFAULT-CDR))
 (1 1 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(TYPES::TYPE-FIX!)
(TYPES::SYMBOLP-SYMBOLP-SYMBOLP-SYMBOLP-IMPLIES-SYMBOLP-TYPE-FIX!)
(TYPES::TYPE-FIX!)
(TYPES::TYPE-FIX)
(TYPES::SYMBOLP-SYMBOLP-SYMBOLP-SYMBOLP-IMPLIES-SYMBOLP-TYPE-FIX)
(TYPES::TYPE-FIX
 (4 4 (:REWRITE DEFAULT-CAR))
 (2 2 (:REWRITE DEFAULT-CDR))
 )
(TYPES::REPLACE-ASSOC)
(TYPES::T-T-ALISTP-IMPLIES-ALISTP-REPLACE-ASSOC
 (259 19 (:REWRITE OMAP::ALISTP-WHEN-MAPP))
 (107 19 (:REWRITE OMAP::MFIX-IMPLIES-MAPP))
 (95 19 (:REWRITE OMAP::MAPP-WHEN-NOT-EMPTY))
 (76 76 (:TYPE-PRESCRIPTION OMAP::MAPP))
 (50 38 (:TYPE-PRESCRIPTION OMAP::MFIX))
 (38 38 (:TYPE-PRESCRIPTION OMAP::EMPTY))
 (38 19 (:REWRITE OMAP::MFIX-WHEN-MAPP))
 (38 19 (:REWRITE OMAP::MAPP-NON-NIL-IMPLIES-NON-EMPTY))
 (29 26 (:REWRITE DEFAULT-CAR))
 (16 13 (:REWRITE DEFAULT-CDR))
 )
(TYPES::REPLACE-ASSOC
 (91 7 (:REWRITE OMAP::ALISTP-WHEN-MAPP))
 (35 7 (:REWRITE OMAP::MFIX-IMPLIES-MAPP))
 (35 7 (:REWRITE OMAP::MAPP-WHEN-NOT-EMPTY))
 (28 28 (:TYPE-PRESCRIPTION OMAP::MAPP))
 (14 14 (:TYPE-PRESCRIPTION OMAP::MFIX))
 (14 14 (:TYPE-PRESCRIPTION OMAP::EMPTY))
 (14 7 (:REWRITE OMAP::MFIX-WHEN-MAPP))
 (14 7 (:REWRITE OMAP::MAPP-NON-NIL-IMPLIES-NON-EMPTY))
 (8 8 (:REWRITE DEFAULT-CAR))
 (4 4 (:REWRITE DEFAULT-CDR))
 )
(TYPES::GET-KEY-KEYLIST
 (121 41 (:REWRITE DEFAULT-+-2))
 (75 41 (:REWRITE DEFAULT-+-1))
 (40 8 (:REWRITE COMMUTATIVITY-OF-+))
 (32 8 (:DEFINITION INTEGER-ABS))
 (32 4 (:DEFINITION LENGTH))
 (20 4 (:DEFINITION LEN))
 (16 16 (:REWRITE DEFAULT-CDR))
 (13 9 (:REWRITE DEFAULT-<-2))
 (12 12 (:REWRITE FOLD-CONSTS-IN-+))
 (11 9 (:REWRITE DEFAULT-<-1))
 (10 10 (:REWRITE DEFAULT-CAR))
 (8 8 (:REWRITE DEFAULT-UNARY-MINUS))
 (4 4 (:TYPE-PRESCRIPTION LEN))
 (4 4 (:REWRITE DEFAULT-REALPART))
 (4 4 (:REWRITE DEFAULT-NUMERATOR))
 (4 4 (:REWRITE DEFAULT-IMAGPART))
 (4 4 (:REWRITE DEFAULT-DENOMINATOR))
 (4 4 (:REWRITE DEFAULT-COERCE-2))
 (4 4 (:REWRITE DEFAULT-COERCE-1))
 (1 1 (:LINEAR ACL2-COUNT-OF-CONSP-POSITIVE))
 )
(TYPES::GET-KEY-KEYLIST)
(TYPES::FTY-ADD-FIX!)
(TYPES::DEF-FTY-TYPE-FN)