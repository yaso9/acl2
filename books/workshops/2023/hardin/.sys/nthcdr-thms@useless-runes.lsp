(NTHCDR-0--THM
 (34 17 (:TYPE-PRESCRIPTION TRUE-LISTP-NTHCDR-TYPE-PRESCRIPTION))
 (17 17 (:TYPE-PRESCRIPTION TRUE-LISTP))
 )
(CAR-NTHCDR--THM
 (47 14 (:REWRITE DEFAULT-CAR))
 (32 32 (:REWRITE DEFAULT-+-2))
 (32 32 (:REWRITE DEFAULT-+-1))
 (32 14 (:REWRITE ZP-OPEN))
 (18 6 (:REWRITE FOLD-CONSTS-IN-+))
 (6 6 (:REWRITE DEFAULT-<-2))
 (6 6 (:REWRITE DEFAULT-<-1))
 )
(CONSP-NTHCDR--THM
 (18 12 (:REWRITE DEFAULT-+-2))
 (17 11 (:REWRITE DEFAULT-<-2))
 (17 11 (:REWRITE DEFAULT-<-1))
 (12 12 (:REWRITE DEFAULT-+-1))
 (6 6 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (5 5 (:REWRITE ZP-OPEN))
 (4 4 (:REWRITE DEFAULT-CDR))
 )
(LEN-NTHCDR-LE-LEN-LST--THM
 (40 27 (:REWRITE DEFAULT-+-2))
 (32 6 (:REWRITE CONSP-NTHCDR--THM))
 (27 27 (:REWRITE DEFAULT-+-1))
 (22 14 (:REWRITE DEFAULT-<-2))
 (21 14 (:REWRITE DEFAULT-<-1))
 (13 7 (:REWRITE ZP-OPEN))
 (8 8 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (6 2 (:REWRITE FOLD-CONSTS-IN-+))
 (2 2 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 )
(NTHCDR-OF-LEN-LST--THM
 (33 27 (:REWRITE DEFAULT-+-2))
 (27 27 (:REWRITE DEFAULT-+-1))
 (14 9 (:REWRITE DEFAULT-<-2))
 (9 9 (:REWRITE DEFAULT-CDR))
 (9 9 (:REWRITE DEFAULT-<-1))
 )
(NTHCDR-CDR--THM
 (8 8 (:REWRITE DEFAULT-+-2))
 (8 8 (:REWRITE DEFAULT-+-1))
 (5 5 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE ZP-OPEN))
 (1 1 (:REWRITE DEFAULT-<-2))
 (1 1 (:REWRITE DEFAULT-<-1))
 (1 1 (:META CANCEL_PLUS-LESSP-CORRECT))
 )
(CDR-NTHCDR--THM
 (113 21 (:REWRITE DEFAULT-CDR))
 (66 66 (:TYPE-PRESCRIPTION |x < y  =>  0 < y-x|))
 (66 4 (:REWRITE CONSP-NTHCDR--THM))
 (32 28 (:REWRITE DEFAULT-+-2))
 (28 28 (:REWRITE DEFAULT-+-1))
 (20 4 (:DEFINITION LEN))
 (11 7 (:REWRITE DEFAULT-<-2))
 (9 7 (:REWRITE DEFAULT-<-1))
 (7 7 (:META CANCEL_PLUS-LESSP-CORRECT))
 (3 3 (:REWRITE ZP-OPEN))
 (3 3 (:META CANCEL_TIMES-EQUAL-CORRECT))
 (3 3 (:META CANCEL_PLUS-EQUAL-CORRECT))
 )
(CONS-NTHCDR--THM
 (82 82 (:TYPE-PRESCRIPTION |x < y  =>  0 < -x+y|))
 (46 40 (:REWRITE DEFAULT-+-2))
 (40 40 (:REWRITE DEFAULT-+-1))
 (21 21 (:REWRITE DEFAULT-CDR))
 (18 12 (:REWRITE DEFAULT-<-2))
 (13 12 (:REWRITE DEFAULT-<-1))
 (12 12 (:META CANCEL_PLUS-LESSP-CORRECT))
 (6 6 (:REWRITE ZP-OPEN))
 (3 3 (:REWRITE DEFAULT-CAR))
 (3 3 (:META CANCEL_TIMES-EQUAL-CORRECT))
 (3 3 (:META CANCEL_PLUS-EQUAL-CORRECT))
 )
(NTHCDR-OF-CONS--THM
 (27 27 (:TYPE-PRESCRIPTION |x < y  =>  0 < -x+y|))
 (17 3 (:REWRITE ZP-OPEN))
 (12 10 (:REWRITE DEFAULT-+-2))
 (12 10 (:REWRITE DEFAULT-+-1))
 (8 2 (:REWRITE <-0-+-NEGATIVE-1))
 (6 2 (:REWRITE FOLD-CONSTS-IN-+))
 (4 4 (:META CANCEL_PLUS-LESSP-CORRECT))
 (3 3 (:REWRITE DEFAULT-CDR))
 (2 2 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE DEFAULT-<-1))
 )
(APPEND-NTHCDR--THM
 (63 21 (:TYPE-PRESCRIPTION BINARY-APPEND))
 (18 17 (:REWRITE DEFAULT-+-2))
 (17 17 (:REWRITE DEFAULT-+-1))
 (10 10 (:REWRITE DEFAULT-CDR))
 (8 2 (:DEFINITION NTH))
 (5 1 (:DEFINITION LEN))
 (2 1 (:REWRITE DEFAULT-<-2))
 (1 1 (:REWRITE ZP-OPEN))
 (1 1 (:REWRITE DEFAULT-CAR))
 (1 1 (:REWRITE DEFAULT-<-1))
 (1 1 (:META CANCEL_PLUS-LESSP-CORRECT))
 )
(CDR-NTHCDR-MINUS-1--THM
 (35 3 (:DEFINITION NTHCDR))
 (27 27 (:TYPE-PRESCRIPTION |x < y  =>  0 < -x+y|))
 (13 3 (:REWRITE COMMUTATIVITY-OF-+))
 (9 8 (:REWRITE DEFAULT-+-2))
 (9 8 (:REWRITE DEFAULT-+-1))
 (9 2 (:REWRITE ZP-OPEN))
 (4 1 (:REWRITE <-0-+-NEGATIVE-1))
 (3 3 (:REWRITE DEFAULT-CDR))
 (3 1 (:REWRITE FOLD-CONSTS-IN-+))
 (2 2 (:META CANCEL_PLUS-LESSP-CORRECT))
 (1 1 (:REWRITE DEFAULT-<-2))
 (1 1 (:REWRITE DEFAULT-<-1))
 )
(NTHCDR-LEN-MINUS-2--THM
 (166 112 (:REWRITE DEFAULT-+-2))
 (150 150 (:REWRITE DEFAULT-CDR))
 (117 112 (:REWRITE DEFAULT-+-1))
 (72 72 (:REWRITE DEFAULT-CAR))
 (44 44 (:META CANCEL_TIMES-EQUAL-CORRECT))
 (30 22 (:REWRITE DEFAULT-<-2))
 (23 22 (:REWRITE DEFAULT-<-1))
 (8 1 (:REWRITE <-+-NEGATIVE-0-1))
 )
(NTHCDR-MEMBER-NTH--THM
 (12 2 (:DEFINITION NTH))
 (8 1 (:DEFINITION NTHCDR))
 (6 5 (:REWRITE DEFAULT-+-2))
 (6 3 (:TYPE-PRESCRIPTION TRUE-LISTP-NTHCDR-TYPE-PRESCRIPTION))
 (5 5 (:REWRITE DEFAULT-+-1))
 (5 1 (:DEFINITION LEN))
 (4 4 (:REWRITE DEFAULT-CDR))
 (4 2 (:REWRITE DEFAULT-<-1))
 (3 3 (:TYPE-PRESCRIPTION TRUE-LISTP))
 (3 3 (:REWRITE ZP-OPEN))
 (3 2 (:REWRITE DEFAULT-<-2))
 (3 1 (:REWRITE COMMUTATIVITY-OF-+))
 (2 2 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (2 2 (:REWRITE DEFAULT-CAR))
 (2 2 (:META CANCEL_PLUS-LESSP-CORRECT))
 )
(NTHCDR-MEMBER-NTH-1--THM
 (247 188 (:REWRITE DEFAULT-+-2))
 (188 188 (:REWRITE DEFAULT-+-1))
 (138 127 (:REWRITE DEFAULT-CDR))
 (69 49 (:REWRITE DEFAULT-<-2))
 (68 68 (:TYPE-PRESCRIPTION |x < y  =>  0 < y-x|))
 (64 49 (:REWRITE DEFAULT-<-1))
 (35 35 (:REWRITE RATIONALP-IMPLIES-ACL2-NUMBERP))
 (35 35 (:REWRITE DEFAULT-CAR))
 (16 16 (:META CANCEL_TIMES-EQUAL-CORRECT))
 (16 16 (:META CANCEL_PLUS-EQUAL-CORRECT))
 (16 4 (:REWRITE <-0-+-NEGATIVE-2))
 (9 9 (:TYPE-PRESCRIPTION |x < y  =>  0 < -x+y|))
 (2 2 (:REWRITE CAR-CONS))
 )
(NTHCDR-LEN-MINUS-1--THM
 (111 69 (:REWRITE DEFAULT-+-2))
 (73 69 (:REWRITE DEFAULT-+-1))
 (68 68 (:REWRITE DEFAULT-CDR))
 (48 10 (:REWRITE <-0-+-NEGATIVE-1))
 (31 17 (:REWRITE DEFAULT-<-2))
 (28 28 (:META CANCEL_PLUS-LESSP-CORRECT))
 (22 22 (:REWRITE DEFAULT-CAR))
 (18 17 (:REWRITE DEFAULT-<-1))
 (12 12 (:META CANCEL_TIMES-EQUAL-CORRECT))
 (12 12 (:META CANCEL_PLUS-EQUAL-CORRECT))
 (8 1 (:REWRITE <-+-NEGATIVE-0-1))
 )
(NTHCDR-PRESERVES-TRUE-LISTP--THM)
(NTHCDR-GE-LEN-NIL--THM)
(NTHCDR-LT-LEN-NOT-NIL--THM
 (10 5 (:TYPE-PRESCRIPTION TRUE-LISTP-NTHCDR-TYPE-PRESCRIPTION))
 (5 5 (:TYPE-PRESCRIPTION TRUE-LISTP))
 )
(LEN-NTHCDR-MINUS-1-GT-LEN-NTHCDR--THM
 (140 6 (:DEFINITION NTHCDR))
 (79 16 (:REWRITE NTHCDR-GE-LEN-NIL--THM))
 (45 9 (:DEFINITION TRUE-LISTP))
 (32 6 (:REWRITE COMMUTATIVITY-OF-+))
 (31 25 (:REWRITE DEFAULT-+-2))
 (26 25 (:REWRITE DEFAULT-+-1))
 (18 18 (:REWRITE DEFAULT-CDR))
 (13 3 (:REWRITE FOLD-CONSTS-IN-+))
 (10 10 (:TYPE-PRESCRIPTION |x < y  =>  0 < -x+y|))
 (9 2 (:REWRITE ZP-OPEN))
 (8 2 (:REWRITE NTHCDR-CDR--THM))
 (4 2 (:REWRITE UNICITY-OF-0))
 (4 1 (:REWRITE <-0-+-NEGATIVE-1))
 (3 2 (:REWRITE DEFAULT-<-2))
 (2 2 (:REWRITE DEFAULT-<-1))
 )
(NTHCDR-UPDATE-NTH--THM
 (816 53 (:REWRITE NTHCDR-GE-LEN-NIL--THM))
 (566 51 (:DEFINITION TRUE-LISTP))
 (164 112 (:REWRITE DEFAULT-CDR))
 (85 76 (:REWRITE DEFAULT-+-2))
 (80 76 (:REWRITE DEFAULT-+-1))
 (76 17 (:REWRITE ZP-OPEN))
 (32 8 (:REWRITE <-0-+-NEGATIVE-1))
 (31 31 (:META CANCEL_PLUS-LESSP-CORRECT))
 (27 9 (:REWRITE FOLD-CONSTS-IN-+))
 (22 22 (:REWRITE DEFAULT-<-2))
 (22 22 (:REWRITE DEFAULT-<-1))
 (10 10 (:REWRITE DEFAULT-CAR))
 (8 8 (:META CANCEL_TIMES-EQUAL-CORRECT))
 (8 8 (:META CANCEL_PLUS-EQUAL-CORRECT))
 )
