(STR::NONDIGIT-CHAR-P)
(STR::BOOLEANP-OF-NONDIGIT-CHAR-P)
(STR::CHARACTERP-WHEN-NONDIGIT-CHAR-P)
(STR::MAKE-NONDIGIT-CHARS)
(STR::NONDIGIT-CHARS$INLINE)
(STR::CHARSET-P-OF-NONDIGIT-CHARS)
(STR::DEFCHARSET-TESTER)
(STR::DEFCHARSET-LEMMA1
 (55 14 (:REWRITE STR::DEFAULT-CODE-CHAR))
 (46 14 (:REWRITE DEFAULT-CODE-CHAR))
 (38 38 (:REWRITE DEFAULT-<-2))
 (38 38 (:REWRITE DEFAULT-<-1))
 (14 14 (:REWRITE STR::CHAR-IN-CHARSET-P-WHEN-MEMBER-EQUAL-OF-CHARS-IN-CHARSET-P))
 (14 7 (:REWRITE STR::CHAR-IN-CHARSET-P-WHEN-NOT-CHARACTER))
 (4 4 (:REWRITE ZP-OPEN))
 (2 2 (:REWRITE DEFAULT-+-2))
 (2 2 (:REWRITE DEFAULT-+-1))
 )
(STR::DEFCHARSET-LEMMA2)
(STR::CHAR-IN-CHARSET-P-OF-NONDIGIT-CHARS
 (34 4 (:REWRITE STR::DEFAULT-CODE-CHAR))
 (22 4 (:REWRITE DEFAULT-CODE-CHAR))
 (16 2 (:REWRITE ZP-OPEN))
 (15 15 (:REWRITE DEFAULT-CHAR-CODE))
 (13 13 (:REWRITE DEFAULT-<-2))
 (13 13 (:REWRITE DEFAULT-<-1))
 (10 10 (:REWRITE STR::CHAR-IN-CHARSET-P-WHEN-MEMBER-EQUAL-OF-CHARS-IN-CHARSET-P))
 )
(STD::DEFLIST-LOCAL-BOOLEANP-ELEMENT-THM)
(STR::NONDIGIT-CHARLIST-P)
(STR::NONDIGIT-CHARLIST-P-OF-CONS)
(STR::NONDIGIT-CHARLIST-P-OF-CDR-WHEN-NONDIGIT-CHARLIST-P)
(STR::NONDIGIT-CHARLIST-P-WHEN-NOT-CONSP)
(STR::NONDIGIT-CHAR-P-OF-CAR-WHEN-NONDIGIT-CHARLIST-P)
(STR::NONDIGIT-CHARLIST-P-OF-APPEND)
(STR::NONDIGIT-CHARLIST-P-OF-LIST-FIX)
(STR::NONDIGIT-CHARLIST-P-OF-SFIX)
(STR::NONDIGIT-CHARLIST-P-OF-INSERT)
(STR::NONDIGIT-CHARLIST-P-OF-DELETE)
(STR::NONDIGIT-CHARLIST-P-OF-MERGESORT)
(STR::NONDIGIT-CHARLIST-P-OF-UNION)
(STR::NONDIGIT-CHARLIST-P-OF-INTERSECT-1)
(STR::NONDIGIT-CHARLIST-P-OF-INTERSECT-2)
(STR::NONDIGIT-CHARLIST-P-OF-DIFFERENCE)
(STR::NONDIGIT-CHARLIST-P-OF-DUPLICATED-MEMBERS)
(STR::NONDIGIT-CHARLIST-P-OF-REV)
(STR::NONDIGIT-CHARLIST-P-OF-RCONS)
(STR::NONDIGIT-CHAR-P-WHEN-MEMBER-EQUAL-OF-NONDIGIT-CHARLIST-P)
(STR::NONDIGIT-CHARLIST-P-WHEN-SUBSETP-EQUAL)
(STR::NONDIGIT-CHARLIST-P-SET-EQUIV-CONGRUENCE)
(STR::NONDIGIT-CHARLIST-P-OF-SET-DIFFERENCE-EQUAL)
(STR::NONDIGIT-CHARLIST-P-OF-INTERSECTION-EQUAL-1)
(STR::NONDIGIT-CHARLIST-P-OF-INTERSECTION-EQUAL-2)
(STR::NONDIGIT-CHARLIST-P-OF-UNION-EQUAL)
(STR::NONDIGIT-CHARLIST-P-OF-TAKE)
(STR::NONDIGIT-CHARLIST-P-OF-REPEAT)
(STR::NONDIGIT-CHAR-P-OF-NTH-WHEN-NONDIGIT-CHARLIST-P)
(STR::NONDIGIT-CHARLIST-P-OF-UPDATE-NTH)
(STR::NONDIGIT-CHARLIST-P-OF-BUTLAST)
(STR::NONDIGIT-CHARLIST-P-OF-NTHCDR)
(STR::NONDIGIT-CHARLIST-P-OF-LAST)
(STR::NONDIGIT-CHARLIST-P-OF-REMOVE)
(STR::NONDIGIT-CHARLIST-P-OF-REVAPPEND)
(STR::CHARS-IN-CHARSET-P-OF-NONDIGIT-CHARS
 (543 69 (:REWRITE STR::NONDIGIT-CHARLIST-P-OF-CDR-WHEN-NONDIGIT-CHARLIST-P))
 (461 45 (:REWRITE STR::NONDIGIT-CHAR-P-OF-CAR-WHEN-NONDIGIT-CHARLIST-P))
 (284 233 (:REWRITE STR::NONDIGIT-CHARLIST-P-WHEN-SUBSETP-EQUAL))
 (172 94 (:REWRITE STR::NONDIGIT-CHAR-P-WHEN-MEMBER-EQUAL-OF-NONDIGIT-CHARLIST-P))
 (63 63 (:REWRITE DEFAULT-CDR))
 (45 45 (:REWRITE DEFAULT-CAR))
 (45 3 (:DEFINITION MEMBER-EQUAL))
 (42 6 (:REWRITE SUBSETP-CAR-MEMBER))
 (21 3 (:REWRITE SUBSETP-IMPLIES-SUBSETP-CDR))
 (18 3 (:REWRITE STR::CHARS-IN-CHARSET-P-OF-CDR-WHEN-CHARS-IN-CHARSET-P))
 (15 15 (:TYPE-PRESCRIPTION MEMBER-EQUAL))
 (15 15 (:REWRITE SUBSETP-TRANS2))
 (15 15 (:REWRITE SUBSETP-TRANS))
 (12 12 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (12 12 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (6 6 (:REWRITE SUBSETP-MEMBER . 2))
 (6 6 (:REWRITE SUBSETP-MEMBER . 1))
 (3 3 (:REWRITE SUBSETP-REFL))
 )