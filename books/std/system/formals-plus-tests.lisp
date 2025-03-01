; Standard System Library
;
; Copyright (C) 2024 Kestrel Institute (http://www.kestrel.edu)
;
; License: A 3-clause BSD license. See the LICENSE file distributed with ACL2.
;
; Author: Alessandro Coglio (www.alessandrocoglio.info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "ACL2")

(include-book "formals-plus")

(include-book "std/testing/assert-equal" :dir :system)
(include-book "std/testing/must-succeed-star" :dir :system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert-equal (formals+ 'len (w state)) '(x))

(must-succeed*
 (defun f (x y zz aaa b77) (list x y zz aaa b77))
 (assert-equal (formals+ 'f (w state)) '(x y zz aaa b77)))

(assert-equal (formals+ '(lambda (x y) (binary-+ x y)) (w state))
              '(x y))

(assert-equal (formals+ '(lambda () '33) (w state))
              nil)
