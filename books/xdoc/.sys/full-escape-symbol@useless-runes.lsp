(XDOC::BAR-ESCAPE-CHARS
 (1 1 (:REWRITE DEFAULT-CDR))
 (1 1 (:REWRITE DEFAULT-CAR))
 )
(XDOC::CHARACTER-LISTP-OF-BAR-ESCAPE-CHARS
 (27 24 (:REWRITE DEFAULT-CAR))
 (16 13 (:REWRITE DEFAULT-CDR))
 )
(XDOC::BAR-ESCAPE-STRING
 (5 1 (:DEFINITION XDOC::BAR-ESCAPE-CHARS))
 (3 3 (:REWRITE DEFAULT-CDR))
 (3 3 (:REWRITE DEFAULT-CAR))
 (3 1 (:DEFINITION POSITION-EQUAL-AC))
 (2 2 (:REWRITE DEFAULT-COERCE-2))
 (2 2 (:REWRITE DEFAULT-COERCE-1))
 (1 1 (:TYPE-PRESCRIPTION XDOC::BAR-ESCAPE-STRING))
 )
(XDOC::FULL-ESCAPE-SYMBOL
 (34 6 (:DEFINITION XDOC::BAR-ESCAPE-CHARS))
 (24 6 (:REWRITE DEFAULT-COERCE-3))
 (22 22 (:REWRITE DEFAULT-CDR))
 (22 22 (:REWRITE DEFAULT-CAR))
 (22 6 (:DEFINITION POSITION-EQUAL-AC))
 (18 18 (:REWRITE DEFAULT-COERCE-2))
 (16 12 (:REWRITE DEFAULT-COERCE-1))
 (10 10 (:REWRITE DEFAULT-SYMBOL-NAME))
 (4 4 (:REWRITE DEFAULT-SYMBOL-PACKAGE-NAME))
 )