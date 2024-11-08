(FAL-ALL-BOUNDP-FAST)
(FAL-ALL-BOUNDP-SLOW)
(FAL-ALL-BOUNDP)
(FAL-ALL-BOUNDP-FAST-REMOVAL
 (50 50 (:REWRITE DEFAULT-CAR))
 (30 30 (:REWRITE HONS-ASSOC-EQUAL-WHEN-ATOM))
 (24 24 (:REWRITE DEFAULT-CDR))
 )
(FAL-ALL-BOUNDP-SLOW-REMOVAL
 (50 50 (:REWRITE DEFAULT-CAR))
 (30 30 (:REWRITE HONS-ASSOC-EQUAL-WHEN-ATOM))
 (24 24 (:REWRITE DEFAULT-CDR))
 )
(FAL-ALL-BOUNDP
 (50 5 (:DEFINITION HONS-ASSOC-EQUAL))
 (25 25 (:REWRITE DEFAULT-CAR))
 (15 15 (:REWRITE HONS-ASSOC-EQUAL-WHEN-ATOM))
 (15 15 (:REWRITE DEFAULT-CDR))
 (5 5 (:DEFINITION HONS-EQUAL))
 )
(FAL-ALL-BOUNDP-WHEN-ATOM)
(FAL-ALL-BOUNDP-OF-CONS
 (50 5 (:DEFINITION HONS-ASSOC-EQUAL))
 (23 23 (:REWRITE DEFAULT-CAR))
 (15 15 (:REWRITE HONS-ASSOC-EQUAL-WHEN-ATOM))
 (13 13 (:REWRITE DEFAULT-CDR))
 (6 6 (:REWRITE FAL-ALL-BOUNDP-WHEN-ATOM))
 (5 5 (:DEFINITION HONS-EQUAL))
 )
(L0
 (38 38 (:REWRITE DEFAULT-CAR))
 (21 21 (:REWRITE HONS-ASSOC-EQUAL-WHEN-ATOM))
 (18 18 (:REWRITE DEFAULT-CDR))
 (12 6 (:REWRITE SUBSETP-MEMBER . 3))
 (6 6 (:REWRITE SUBSETP-MEMBER . 4))
 (6 6 (:REWRITE SUBSETP-MEMBER . 2))
 (6 6 (:REWRITE SUBSETP-MEMBER . 1))
 (6 6 (:REWRITE INTERSECTP-MEMBER . 3))
 (6 6 (:REWRITE INTERSECTP-MEMBER . 2))
 (2 2 (:TYPE-PRESCRIPTION SUBSETP-EQUAL))
 (1 1 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (1 1 (:REWRITE SUBSETP-WHEN-ATOM-LEFT))
 (1 1 (:REWRITE SUBSETP-TRANS2))
 (1 1 (:REWRITE SUBSETP-TRANS))
 )
(L1
 (24 2 (:DEFINITION HONS-ASSOC-EQUAL))
 (15 1 (:DEFINITION MEMBER-EQUAL))
 (14 14 (:REWRITE DEFAULT-CAR))
 (12 12 (:REWRITE DEFAULT-CDR))
 (11 6 (:REWRITE SUBSETP-TRANS2))
 (6 6 (:REWRITE SUBSETP-TRANS))
 (6 6 (:REWRITE HONS-ASSOC-EQUAL-WHEN-ATOM))
 (5 5 (:REWRITE SUBSETP-WHEN-ATOM-RIGHT))
 (2 2 (:TYPE-PRESCRIPTION HONS-ASSOC-EQUAL))
 (2 2 (:REWRITE SUBSETP-MEMBER . 2))
 (2 2 (:REWRITE SUBSETP-MEMBER . 1))
 (2 2 (:DEFINITION HONS-EQUAL))
 )
(SET-EQUIV-IMPLIES-EQUAL-FAL-ALL-BOUNDP-1
 (57 3 (:DEFINITION FAL-ALL-BOUNDP))
 (36 3 (:DEFINITION HONS-ASSOC-EQUAL))
 (15 15 (:REWRITE DEFAULT-CAR))
 (9 9 (:REWRITE L0))
 (9 9 (:REWRITE HONS-ASSOC-EQUAL-WHEN-ATOM))
 (9 9 (:REWRITE DEFAULT-CDR))
 (6 6 (:REWRITE FAL-ALL-BOUNDP-WHEN-ATOM))
 (3 3 (:TYPE-PRESCRIPTION HONS-ASSOC-EQUAL))
 (3 3 (:DEFINITION HONS-EQUAL))
 )
(ALIST-EQUIV-IMPLIES-EQUAL-FAL-ALL-BOUNDP-2
 (50 50 (:REWRITE DEFAULT-CAR))
 (30 30 (:REWRITE HONS-ASSOC-EQUAL-WHEN-ATOM))
 (24 24 (:REWRITE DEFAULT-CDR))
 )
(FAL-ALL-BOUNDP-OF-LIST-FIX)
(FAL-ALL-BOUNDP-OF-REV)
(FAL-ALL-BOUNDP-OF-APPEND
 (166 166 (:REWRITE DEFAULT-CAR))
 (151 88 (:REWRITE DEFAULT-CDR))
 (144 72 (:TYPE-PRESCRIPTION TRUE-LISTP-APPEND))
 (90 90 (:REWRITE HONS-ASSOC-EQUAL-WHEN-ATOM))
 (72 72 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (72 72 (:TYPE-PRESCRIPTION BINARY-APPEND))
 (15 15 (:REWRITE APPEND-ATOM-UNDER-LIST-EQUIV))
 (9 9 (:REWRITE CDR-OF-APPEND-WHEN-CONSP))
 )
(FAL-ALL-BOUNDP-OF-REVAPPEND
 (64 4 (:DEFINITION FAL-ALL-BOUNDP))
 (40 4 (:DEFINITION HONS-ASSOC-EQUAL))
 (21 21 (:REWRITE DEFAULT-CAR))
 (13 13 (:REWRITE DEFAULT-CDR))
 (12 12 (:REWRITE HONS-ASSOC-EQUAL-WHEN-ATOM))
 (8 8 (:REWRITE FAL-ALL-BOUNDP-WHEN-ATOM))
 (5 1 (:DEFINITION BINARY-APPEND))
 (4 4 (:TYPE-PRESCRIPTION HONS-ASSOC-EQUAL))
 (4 4 (:DEFINITION HONS-EQUAL))
 (2 2 (:REWRITE APPEND-WHEN-NOT-CONSP))
 (2 2 (:REWRITE APPEND-ATOM-UNDER-LIST-EQUIV))
 )
