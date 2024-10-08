; Standard System Library
;
; Copyright (C) 2024 Kestrel Institute (http://www.kestrel.edu)
;
; License: A 3-clause BSD license. See the LICENSE file distributed with ACL2.
;
; Author: Alessandro Coglio (www.alessandrocoglio.info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "ACL2")

(include-book "ubody")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ubody+ ((fn pseudo-termfnp) (wrld plist-worldp))
  :returns (body pseudo-termp)
  :parents (std/system/function-queries)
  :short "Enhanced variant of @(tsee ubody)."
  :long
  (xdoc::topstring-p
   "This returns the same result as @(tsee ubody),
    but it includes a run-time check (which should always succeed) on the result
    that allows us to prove the return type theorem
    without strengthening the guard on @('wrld').
    Furthermore, this utility causes an error if called on
    a symbol that does not name a function.")
  (if (and (symbolp fn)
           (not (function-symbolp fn wrld)))
      (raise "The symbol ~x0 does not name a function." fn)
    (b* ((result (ubody fn wrld)))
      (if (pseudo-termp result)
          result
        (raise "Internal error: ~
                the unnormalized body ~x0 of ~x1 is not a pseudo-term."
               result fn)))))
