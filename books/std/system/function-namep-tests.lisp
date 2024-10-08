; Standard System Library
;
; Copyright (C) 2024 Kestrel Institute (http://www.kestrel.edu)
;
; License: A 3-clause BSD license. See the LICENSE file distributed with ACL2.
;
; Author: Alessandro Coglio (www.alessandrocoglio.info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "ACL2")

(include-book "function-namep")

(include-book "std/testing/assert-bang" :dir :system)
(include-book "std/testing/must-succeed-star" :dir :system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert! (function-namep 'len (w state)))

(assert! (not (function-namep 'cons-car-cdr (w state))))

(assert! (not (function-namep 'bbbbbbbbbbb (w state))))

(must-succeed*
 (defun f (x) x)
 (assert! (function-namep 'f (w state))))

(assert! (not (function-namep 33 (w state))))

(assert! (not (function-namep "len" (w state))))
