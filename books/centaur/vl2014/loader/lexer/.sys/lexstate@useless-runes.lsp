(VL2014::VL-PLAINTOKEN-ALISTP)
(VL2014::VL-PLAINTOKEN-ALISTP-WHEN-ATOM
 (1 1 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-TRUELIST-ALISTP . 2))
 (1 1 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-TRUELIST-ALISTP . 1))
 (1 1 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (1 1 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (1 1 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-TRUELIST-ALISTP . 2))
 (1 1 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-TRUELIST-ALISTP . 1))
 (1 1 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-SYMBOL-ALISTP . 2))
 (1 1 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-SYMBOL-ALISTP . 1))
 )
(VL2014::VL-PLAINTOKEN-ALISTP-OF-CONS
 (46 6 (:REWRITE VL2014::VL-PLAINTOKEN-ALISTP-WHEN-ATOM))
 (22 22 (:REWRITE DEFAULT-CAR))
 (18 2 (:REWRITE CONSP-OF-CAR-WHEN-ALISTP))
 (12 12 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-TRUELIST-ALISTP . 2))
 (12 12 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-TRUELIST-ALISTP . 1))
 (12 12 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (12 12 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (12 12 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-TRUELIST-ALISTP . 2))
 (12 12 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-TRUELIST-ALISTP . 1))
 (12 12 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-SYMBOL-ALISTP . 2))
 (12 12 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-SYMBOL-ALISTP . 1))
 (10 10 (:REWRITE VL2014::VL-PLAINTOKENTYPE-P-WHEN-MEMBER-EQUAL-OF-VL-PLAINTOKENTYPELIST-P))
 (8 8 (:REWRITE DEFAULT-CDR))
 (8 2 (:REWRITE ALISTP-WHEN-HONS-DUPLICITY-ALIST-P))
 (4 4 (:TYPE-PRESCRIPTION HONS-DUPLICITY-ALIST-P))
 (4 4 (:TYPE-PRESCRIPTION ALISTP))
 (2 2 (:REWRITE HONS-DUPLICITY-ALIST-P-WHEN-NOT-CONSP))
 (2 2 (:REWRITE ALISTP-WHEN-ATOM))
 (2 2 (:REWRITE VL2014::ALISTP-WHEN-ALL-HAVE-LEN))
 )
(VL2014::VL-PLAINTOKEN-ALISTP-OF-APPEND
 (527 239 (:REWRITE DEFAULT-CAR))
 (337 58 (:REWRITE DEFAULT-CDR))
 (260 260 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-TRUELIST-ALISTP . 2))
 (260 260 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-TRUELIST-ALISTP . 1))
 (260 260 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (260 260 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (260 260 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-TRUELIST-ALISTP . 2))
 (260 260 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-TRUELIST-ALISTP . 1))
 (260 260 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-SYMBOL-ALISTP . 2))
 (260 260 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-SYMBOL-ALISTP . 1))
 (162 18 (:REWRITE CONSP-OF-CAR-WHEN-ALISTP))
 (144 72 (:TYPE-PRESCRIPTION TRUE-LISTP-APPEND))
 (81 9 (:REWRITE CDR-OF-APPEND-WHEN-CONSP))
 (72 72 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (72 72 (:TYPE-PRESCRIPTION BINARY-APPEND))
 (72 18 (:REWRITE ALISTP-WHEN-HONS-DUPLICITY-ALIST-P))
 (60 60 (:REWRITE VL2014::VL-PLAINTOKENTYPE-P-WHEN-MEMBER-EQUAL-OF-VL-PLAINTOKENTYPELIST-P))
 (36 36 (:TYPE-PRESCRIPTION HONS-DUPLICITY-ALIST-P))
 (36 36 (:TYPE-PRESCRIPTION ALISTP))
 (18 18 (:REWRITE HONS-DUPLICITY-ALIST-P-WHEN-NOT-CONSP))
 (18 18 (:REWRITE ALISTP-WHEN-ATOM))
 (18 18 (:REWRITE VL2014::ALISTP-WHEN-ALL-HAVE-LEN))
 )
(VL2014::VL-PLAINTOKENTYPE-P-OF-HONS-ASSOC-EQUAL-WHEN-VL-PLAINTOKEN-ALISTP
 (225 21 (:REWRITE CONSP-OF-CAR-WHEN-ALISTP))
 (153 153 (:REWRITE DEFAULT-CAR))
 (100 25 (:REWRITE ALISTP-WHEN-HONS-DUPLICITY-ALIST-P))
 (94 59 (:REWRITE DEFAULT-CDR))
 (84 84 (:REWRITE VL2014::VL-PLAINTOKENTYPE-P-WHEN-MEMBER-EQUAL-OF-VL-PLAINTOKENTYPELIST-P))
 (81 81 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-TRUELIST-ALISTP . 2))
 (81 81 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-TRUELIST-ALISTP . 1))
 (81 81 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (81 81 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (81 81 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-TRUELIST-ALISTP . 2))
 (81 81 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-TRUELIST-ALISTP . 1))
 (81 81 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-SYMBOL-ALISTP . 2))
 (81 81 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-SYMBOL-ALISTP . 1))
 (50 50 (:TYPE-PRESCRIPTION HONS-DUPLICITY-ALIST-P))
 (50 50 (:TYPE-PRESCRIPTION ALISTP))
 (36 4 (:REWRITE ALISTP-OF-CDR))
 (25 25 (:REWRITE HONS-DUPLICITY-ALIST-P-WHEN-NOT-CONSP))
 (25 25 (:REWRITE ALISTP-WHEN-ATOM))
 (25 25 (:REWRITE VL2014::ALISTP-WHEN-ALL-HAVE-LEN))
 (25 5 (:REWRITE CONSP-OF-HONS-ASSOC-EQUAL))
 )
(VL2014::VL-LEXSTATE-P)
(VL2014::VL-LEXSTATE)
(VL2014::HONSED-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->KWDTABLE$INLINE)
(VL2014::VL-LEXSTATE->BANGOPS$INLINE)
(VL2014::VL-LEXSTATE->POUNDOPS$INLINE)
(VL2014::VL-LEXSTATE->REMOPS$INLINE)
(VL2014::VL-LEXSTATE->ANDOPS$INLINE)
(VL2014::VL-LEXSTATE->STAROPS$INLINE)
(VL2014::VL-LEXSTATE->PLUSOPS$INLINE)
(VL2014::VL-LEXSTATE->DASHOPS$INLINE)
(VL2014::VL-LEXSTATE->DOTOPS$INLINE)
(VL2014::VL-LEXSTATE->DIVOPS$INLINE)
(VL2014::VL-LEXSTATE->COLONOPS$INLINE)
(VL2014::VL-LEXSTATE->LESSOPS$INLINE)
(VL2014::VL-LEXSTATE->GTOPS$INLINE)
(VL2014::VL-LEXSTATE->EQOPS$INLINE)
(VL2014::VL-LEXSTATE->XOROPS$INLINE)
(VL2014::VL-LEXSTATE->BAROPS$INLINE)
(VL2014::VL-LEXSTATE->DOLLAROPS$INLINE)
(VL2014::VL-LEXSTATE->QUOTESP$INLINE)
(VL2014::VL-LEXSTATE->STREXTSP$INLINE)
(VL2014::VL-LEXSTATE->TIMELITSP$INLINE)
(VL2014::VL-LEXSTATE->EXTINTSP$INLINE)
(VL2014::REMAKE-VL-LEXSTATE)
(VL2014::CONSP-OF-VL-LEXSTATE)
(VL2014::BOOLEANP-OF-VL-LEXSTATE-P)
(VL2014::VL-LEXSTATE-P-OF-VL-LEXSTATE)
(VL2014::CONSP-WHEN-VL-LEXSTATE-P)
(VL2014::VL-LEXSTATE->KWDTABLE-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->BANGOPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->POUNDOPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->REMOPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->ANDOPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->STAROPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->PLUSOPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->DASHOPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->DOTOPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->DIVOPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->COLONOPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->LESSOPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->GTOPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->EQOPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->XOROPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->BAROPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->DOLLAROPS-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->QUOTESP-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->STREXTSP-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->TIMELITSP-OF-VL-LEXSTATE)
(VL2014::VL-LEXSTATE->EXTINTSP-OF-VL-LEXSTATE)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->KWDTABLE)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->BANGOPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->POUNDOPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->REMOPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->ANDOPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->STAROPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->PLUSOPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->DASHOPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->DOTOPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->DIVOPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->COLONOPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->LESSOPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->GTOPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->EQOPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->XOROPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->BAROPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->DOLLAROPS)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->QUOTESP)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->STREXTSP)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->TIMELITSP)
(VL2014::RETURN-TYPE-OF-VL-LEXSTATE->EXTINTSP)
(VL2014::VL-LEXSTATE->PLAINALIST
 (208 208 (:TYPE-PRESCRIPTION BINARY-APPEND-WITHOUT-GUARD))
 )
(VL2014::VL-PLAINTOKEN-ALISTP-OF-VL-LEXSTATE->PLAINALIST
 (300 15 (:DEFINITION BINARY-APPEND))
 (270 30 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (45 45 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-TRUELIST-ALISTP . 2))
 (45 45 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-TRUELIST-ALISTP . 1))
 (45 45 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2))
 (45 45 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 1))
 (45 45 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-TRUELIST-ALISTP . 2))
 (45 45 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-TRUELIST-ALISTP . 1))
 (45 45 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-SYMBOL-ALISTP . 2))
 (45 45 (:REWRITE CONSP-WHEN-MEMBER-EQUAL-OF-KEYWORD-SYMBOL-ALISTP . 1))
 (15 15 (:REWRITE DEFAULT-CDR))
 (15 15 (:REWRITE DEFAULT-CAR))
 )
(VL2014::VL-LEXSTATE-INIT)
(VL2014::VL-LEXSTATE-P-OF-VL-LEXSTATE-INIT)
