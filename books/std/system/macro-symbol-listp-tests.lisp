; Standard System Library
;
; Copyright (C) 2024 Kestrel Institute (http://www.kestrel.edu)
;
; License: A 3-clause BSD license. See the LICENSE file distributed with ACL2.
;
; Author: Alessandro Coglio (www.alessandrocoglio.info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "ACL2")

(include-book "macro-symbol-listp")

(include-book "std/testing/assert-bang" :dir :system)
(include-book "std/testing/must-succeed-star" :dir :system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert! (macro-symbol-listp nil (w state)))

(assert! (macro-symbol-listp '(append + * *) (w state)))

(assert! (not (macro-symbol-listp '(append binary-+) (w state))))

(must-succeed*
 (defmacro m (x) `(list ,x))
 (defmacro n (x) `(cons ,x ,x))
 (assert! (macro-symbol-listp '(m n append) (w state))))
