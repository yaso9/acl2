(SYMBOL-LISTP-OF-STRIP-CARS-WHEN-SYMBOL-TERM-ALISTP
 (14 10 (:REWRITE DEFAULT-CAR))
 (7 6 (:REWRITE DEFAULT-CDR))
 (3 3 (:REWRITE CONSP-OF-CAR-WHEN-SYMBOL-TERM-ALISTP-CHEAP))
 )
(SYMBOL-TERM-ALISTP-OF-APPEND
 (285 5 (:DEFINITION PSEUDO-TERMP))
 (86 86 (:REWRITE DEFAULT-CDR))
 (85 85 (:REWRITE DEFAULT-CAR))
 (85 15 (:DEFINITION LEN))
 (66 33 (:TYPE-PRESCRIPTION TRUE-LISTP-APPEND))
 (38 19 (:REWRITE CONSP-OF-CAR-WHEN-SYMBOL-TERM-ALISTP-CHEAP))
 (35 35 (:TYPE-PRESCRIPTION LEN))
 (33 33 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (33 33 (:TYPE-PRESCRIPTION BINARY-APPEND))
 (30 15 (:REWRITE DEFAULT-+-2))
 (25 5 (:DEFINITION SYMBOL-LISTP))
 (20 5 (:DEFINITION TRUE-LISTP))
 (15 15 (:REWRITE DEFAULT-+-1))
 (13 5 (:REWRITE PSEUDO-TERMP-OF-CDR-OF-CAR-WHEN-SYMBOL-TERM-ALISTP))
 (10 10 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (5 5 (:TYPE-PRESCRIPTION SYMBOL-LISTP))
 (5 5 (:TYPE-PRESCRIPTION PSEUDO-TERM-LISTP))
 (5 5 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 )
(SYMBOL-TERM-ALISTP-OF-TRUE-LIST-FIX
 (145 144 (:REWRITE DEFAULT-CAR))
 (136 24 (:DEFINITION LEN))
 (131 130 (:REWRITE DEFAULT-CDR))
 (48 24 (:REWRITE DEFAULT-+-2))
 (46 8 (:DEFINITION SYMBOL-LISTP))
 (38 8 (:DEFINITION TRUE-LISTP))
 (24 24 (:REWRITE DEFAULT-+-1))
 (16 16 (:REWRITE TERMP-IMPLIES-PSEUDO-TERMP))
 (8 8 (:REWRITE TERM-LISTP-IMPLIES-PSEUDO-TERM-LISTP))
 )
(SYMBOL-TERM-ALIST-LISTP)
(SYMBOL-TERM-ALISTP-OF-CAR-WHEN-SYMBOL-TERM-ALIST-LISTP
 (1 1 (:REWRITE DEFAULT-CAR))
 )
(SYMBOL-TERM-ALIST-LISTP-OF-CDR-WHEN-SYMBOL-TERM-ALIST-LISTP
 (4 1 (:REWRITE SYMBOL-TERM-ALISTP-OF-CAR-WHEN-SYMBOL-TERM-ALIST-LISTP))
 (1 1 (:REWRITE DEFAULT-CDR))
 (1 1 (:REWRITE DEFAULT-CAR))
 )
(SYMBOL-TERM-ALIST-LISTP-FORWARD-TO-TRUE-LISTP
 (8 2 (:REWRITE SYMBOL-TERM-ALISTP-OF-CAR-WHEN-SYMBOL-TERM-ALIST-LISTP))
 (8 2 (:REWRITE SYMBOL-TERM-ALIST-LISTP-OF-CDR-WHEN-SYMBOL-TERM-ALIST-LISTP))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 2 (:DEFINITION TRUE-LISTP))
 (2 2 (:REWRITE DEFAULT-CAR))
 )
(SYMBOL-TERM-ALIST-LISTP-OF-CONS
 (8 2 (:REWRITE SYMBOL-TERM-ALISTP-OF-CAR-WHEN-SYMBOL-TERM-ALIST-LISTP))
 (8 2 (:REWRITE SYMBOL-TERM-ALIST-LISTP-OF-CDR-WHEN-SYMBOL-TERM-ALIST-LISTP))
 (3 3 (:REWRITE DEFAULT-CDR))
 (3 3 (:REWRITE DEFAULT-CAR))
 (2 2 (:DEFINITION NULL))
 )
(SYMBOL-TERM-ALIST-LISTP-OF-UPDATE-NTH
 (25 17 (:REWRITE DEFAULT-CAR))
 (20 12 (:REWRITE DEFAULT-CDR))
 (8 5 (:REWRITE ZP-OPEN))
 (6 6 (:REWRITE DEFAULT-+-2))
 (6 6 (:REWRITE DEFAULT-+-1))
 (3 1 (:REWRITE FOLD-CONSTS-IN-+))
 (1 1 (:REWRITE DEFAULT-<-2))
 (1 1 (:REWRITE DEFAULT-<-1))
 )
(SYMBOL-TERM-ALISTP-OF-NTH-WHEN-SYMBOL-TERM-ALIST-LISTP
 (12 12 (:REWRITE DEFAULT-CAR))
 (8 2 (:REWRITE SYMBOL-TERM-ALIST-LISTP-OF-CDR-WHEN-SYMBOL-TERM-ALIST-LISTP))
 (4 4 (:REWRITE ZP-OPEN))
 (4 4 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:REWRITE DEFAULT-+-1))
 )