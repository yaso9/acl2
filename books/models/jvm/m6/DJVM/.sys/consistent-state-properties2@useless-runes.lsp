(DJVM::CONSISTENT-STATE-IMLIES-THREAD-BY-ID)
(DJVM::CONSISTENT-STATE-IMPLIES-CONSISTENT-THREAD-ENTRIES)
(DJVM::CONSISTENT-THREAD-ENTRIES-IMPLIES-CONSISTENT-THREAD
 (7 7 (:REWRITE DEFAULT-CDR))
 (6 6 (:REWRITE DEFAULT-CAR))
 (6 3 (:DEFINITION NTH))
 (3 1 (:DEFINITION DJVM::CONSISTENT-THREAD-ENTRIES))
 )
(DJVM::CONSISTENT-THREAD-ENTRY-IMPLIES-CONSP-THREAD-CALL-STACK)
(DJVM::SAFE-TOPSTACK-IS-TOPSTACK
 (2 2 (:REWRITE DEFAULT-CAR))
 )
(DJVM::CONSISTENT-STATE-TOPSTACK-CONSISTENT-VALUE-X-2
 (60 60 (:TYPE-PRESCRIPTION JVM::LOADER-INV))
 (40 20 (:TYPE-PRESCRIPTION JVM::LOADER-INV-IMPLIES-WFF-STATE))
 (40 20 (:TYPE-PRESCRIPTION JVM::LOADER-INV-IMPLIES-WFF-INSTANCE-CLASS-TABLE))
 (40 20 (:TYPE-PRESCRIPTION JVM::LOADER-INV-IMPLIES-WFF-CLASS-TABLE))
 )
