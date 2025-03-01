; Big Memory-Asymmetric Model
; Copyright (C) 2021 Centaur Technology
; Copyright (C) 2024 Intel Corporation
;
; Contact:
;   ACL2 Formal Verification Group
;   Intel Corporation
;   1300 South MoPac Expy,  Austin, TX  78746, USA
;   https://www.intel.com/
;
; License: (An MIT/X11-style license)
;
;   Permission is hereby granted, free of charge, to any person obtaining a
;   copy of this software and associated documentation files (the "Software"),
;   to deal in the Software without restriction, including without limitation
;   the rights to use, copy, modify, merge, publish, distribute, sublicense,
;   and/or sell copies of the Software, and to permit persons to whom the
;   Software is furnished to do so, subject to the following conditions:
;
;   The above copyright notice and this permission notice shall be included in
;   all copies or substantial portions of the Software.
;
;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;   DEALINGS IN THE SOFTWARE.

;   Original Author(s): Shilpi Goel <shilpi@centtech.com>
;   Modified by "Warren A. Hunt, Jr." <warren.hunt@intel.com>

(in-package "ACL2")

(include-book "centaur/bitops/portcullis" :dir :system)
(include-book "centaur/fty/portcullis" :dir :system)
(include-book "std/portcullis" :dir :system)
(include-book "../portcullis")

(defpkg "BIGMEM-ASYMMETRIC"
  (union-eq
   '(
     binary-logand
     binary-logior
     binary-logxor

     ;; TOOLS
     b*
     fty::defprod
     fty::defbitstruct
     def-ruleset
     def-ruleset!
     add-to-ruleset
     ruleset-theory
     enable*
     disable*
     e/d*

     bigmems::ubp8-get
     bigmems::ubp8-get1
     bigmems::ubp8-fix
     bigmems::ubp8-p
     bigmems::ubp8-p1
     bigmems::ubp8-to-mtr

     ;; XDOC
     defsection
     defxdoc)
   (union-eq *acl2-exports*
             bigmems::*bigmems-exports*
             acl2::*bitops-exports*
             std::*std-exports*
             *common-lisp-symbols-from-main-lisp-package*)))

;; ======================================================================
