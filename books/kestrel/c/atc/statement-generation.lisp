; C Library
;
; Copyright (C) 2022 Kestrel Institute (http://www.kestrel.edu)
; Copyright (C) 2022 Kestrel Technology LLC (http://kestreltechnology.com)
;
; License: A 3-clause BSD license. See the LICENSE file distributed with ACL2.
;
; Author: Alessandro Coglio (coglio@kestrel.edu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "C")

(include-book "expression-generation")
(include-book "object-tables")

(local (include-book "std/typed-lists/pseudo-term-listp" :dir :system))
(local (include-book "std/typed-lists/symbol-listp" :dir :system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defxdoc+ atc-statement-generation
  :parents (atc-event-and-code-generation)
  :short "Generation of C statements."
  :order-subtopics t
  :default-parent t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atc-var-assignablep ((var symbolp)
                             (innermostp booleanp)
                             (affect symbol-listp))
  :returns (yes/no booleanp :hyp (booleanp innermostp))
  :short "Check if a variable is assignable,
          based on whether it is in the innermost scope
          and based on the variables being currently affected."
  :long
  (xdoc::topstring
   (xdoc::p
    "A variable may be destructively assigned to
     if any of the following conditions apply:
     (i) it is declared in the innermost scope,
     because in that case it cannot be accessed after exiting the scope;
     (ii) it is being affected,
     because in that case its modified value is returned
     and used in subsequent code;
     (iii) no variable is being affected,
     because in that case there is no subsequent code."))
  (or innermostp
      (and (member-eq var affect) t)
      (null affect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atc-vars-assignablep ((var-list symbol-listp)
                              (innermostp-list boolean-listp)
                              (affect symbol-listp))
  :guard (equal (len var-list) (len innermostp-list))
  :returns (yes/no booleanp :hyp (boolean-listp innermostp-list))
  :short "Lift @(tsee atc-var-assignablep) to lists."
  (or (endp var-list)
      (and
       (atc-var-assignablep (car var-list) (car innermostp-list) affect)
       (atc-vars-assignablep (cdr var-list) (cdr innermostp-list) affect))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atc-affecting-term-for-let-p ((term pseudo-termp)
                                      (prec-fns atc-symbol-fninfo-alistp))
  :returns (yes/no booleanp)
  :short "Check if a term @('term') has the basic structure
          required for representing code affecting variables
          in @('(let ((var term)) body)')
          or @('(mv-let (var1 ... varn) term body)')."
  :long
  (xdoc::topstring
   (xdoc::p
    "This is explained in the user documentation.
     Here we perform a shallow check,
     because we examine the term in full detail
     when recursively generating C code from it.
     In essence, here we check that the term is either
     (i) an @(tsee if) whose test is not @(tsee mbt) or @(tsee mbt$) or
     (ii) a call of a (preceding) target function."))
  (case-match term
    (('if test . &) (and (case-match test
                           ((fn . &) (not (member-eq fn '(mbt mbt$))))
                           (& t))))
    ((fn . &) (and (symbolp fn)
                   (consp (assoc-eq fn prec-fns))))
    (& nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atc-make-mv-nth-terms ((indices nat-listp) (term pseudo-termp))
  :returns (terms pseudo-term-listp)
  :short "Create a list of @(tsee mv-nth)s applied to a term
          for a list of indices."
  (cond ((endp indices) nil)
        (t (cons `(mv-nth ',(car indices) ,(pseudo-term-fix term))
                 (atc-make-mv-nth-terms (cdr indices) term))))
  ///
  (defret len-of-atc-make-mv-nth-terms
    (equal (len terms)
           (len indices))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atc-update-var-term-alist ((vars symbol-listp)
                                   (terms pseudo-term-listp)
                                   (alist symbol-pseudoterm-alistp))
  :returns (new-alist symbol-pseudoterm-alistp)
  :short "Update an alist from symbols to terms."
  (append (pairlis$ (symbol-list-fix vars)
                    (pseudo-term-list-fix terms))
          (symbol-pseudoterm-alist-fix alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atc-ensure-formals-not-lost ((bind-affect symbol-listp)
                                     (fn-affect symbol-listp)
                                     (fn-typed-formals atc-symbol-varinfo-alistp)
                                     (fn symbolp)
                                     (wrld plist-worldp))
  :returns erp
  :short "Ensure that no affected formals are lost."
  :long
  (xdoc::topstring
   (xdoc::p
    "If the body of a non-recursive function @('fn')
     includes an @(tsee mv-let)s or a @(tsee let)
     that affects a formal of @('fn') of pointer type,
     that formal must be among the variables affected by ('fn').
     If the body of a recursive function @('fn')
     includes an @(tsee mv-let)s or a @(tsee let)
     that affects a formal of @('fn') of any type,
     that formal must be among the variables affected by ('fn').
     In other words, no modification of formals must be ``lost''.
     The case of formals of pointer types is clear,
     because it means that objects in the heap are affected.
     The case of formals of non-pointer types
     applies to recursive functions
     because they represent loops,
     which may affect local variables in the function where they appear.")
   (xdoc::p
    "This ACL2 function ensures that no formals are lost in the sense above.
     The parameter @('bind-affect') consists of
     the variable affected by the @(tsee mv-let) or @(tsee let).
     The parameter @('fn-affect') consists of
     the variables purported to be affected by @('fn').
     We go through the elements of @('bind-affect')
     and check each one against the formals of @('fn'),
     taking into account the types and whether @('fn') is recursive."))
  (b* (((reterr))
       ((when (endp bind-affect)) (retok))
       (var (car bind-affect))
       (info (cdr (assoc-eq var fn-typed-formals)))
       ((when (and info
                   (or (irecursivep+ fn wrld)
                       (type-case (atc-var-info->type info) :pointer))
                   (not (member-eq var fn-affect))))
        (reterr
         (msg "When generating C code for the function ~x0, ~
               the formal parameter ~x1 is being affected ~
               in an MV-LET or LET term, ~
               but it is not being returned by ~x0."
              fn var))))
    (atc-ensure-formals-not-lost (cdr bind-affect)
                                 fn-affect
                                 fn-typed-formals
                                 fn
                                 wrld)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fty::defprod stmt-gin
  :short "Inputs for @(tsee atc-gen-stmt)."
  ((context atc-contextp)
   (var-term-alist symbol-pseudoterm-alist)
   (typed-formals atc-symbol-varinfo-alist)
   (inscope atc-symbol-varinfo-alist-list)
   (loop-flag booleanp)
   (affect symbol-list)
   (fn symbolp)
   (fn-guard symbol)
   (compst-var symbol)
   (fenv-var symbol)
   (limit-var symbol)
   (prec-fns atc-symbol-fninfo-alist)
   (prec-tags atc-string-taginfo-alist)
   (prec-objs atc-string-objinfo-alist)
   (thm-index pos)
   (names-to-avoid symbol-list)
   (proofs bool))
  :pred stmt-ginp)

;;;;;;;;;;;;;;;;;;;;

(fty::defprod stmt-gout
  :short "Outputs for @(tsee atc-gen-stmt)."
  ((items block-item-list)
   (type type)
   (limit pseudo-term)
   (events pseudo-event-form-list)
   (thm-name symbol)
   (thm-index pos)
   (names-to-avoid symbol-list)
   (proofs bool))
  :pred stmt-goutp)

;;;;;;;;;;

(defirrelevant irr-stmt-gout
  :short "An irrelevant output for @(tsee atc-gen-stmt)."
  :type stmt-goutp
  :body (make-stmt-gout :items nil
                        :type (irr-type)
                        :limit nil
                        :events nil
                        :thm-name nil
                        :thm-index 1
                        :names-to-avoid nil
                        :proofs nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atc-gen-return-stmt ((term pseudo-termp)
                             (gin stmt-ginp)
                             (must-affect symbol-listp)
                             state)
  :returns (mv erp (gout stmt-goutp))
  :short "Generate a C return statement from an ACL2 term."
  :long
  (xdoc::topstring
   (xdoc::p
    "The term passed here as parameter is the one representing
     the expression to be returned by the statement.
     The @('must-affect') parameter contains the variables
     that must be affected by the expression:
     it is set differently in the two circumstances in @(tsee atc-gen-stmt)
     in which this @('atc-gen-return-stmt') is called,
     corresponding to the two possible representations of @('return') statements
     according to the user documentation.")
   (xdoc::p
    "We generate three theorems, which build upon each other:
     one for @(tsee exec-stmt) applied to the return statement,
     one for @(tsee exec-block-item) applied to
     the block item that consists of the return statement,
     and one for @(tsee exec-block-item-list) applied to
     the singleton list of that block item.
     It is the latter term that refers to the list of block items
     returned as the @('gout') result of this ACL2 function.
     We start with the first of the three theorems,
     we will add the other two next.")
   (xdoc::p
    "The limit for the @(tsee exec-stmt) theorem is set to
     1 more than the limit for the expression theorem,
     because we need 1 to go from @(tsee exec-stmt)
     to the @(':return') case and @(tsee exec-expr-call-or-pure).
     The limit for the @(tsee exec-block-item) theorem is set to
     1 more than the limit for the previous theorem,
     because we need 1 to go from @(tsee exec-block-item)
     to the @(':stmt') case and @(tsee exec-stmt).
     The limit for the @(tsee exec-block-item-list) theorem is set to
     1 more than the limit for the previous theorem,
     because we need 1 to go from @(tsee exec-block-item-list)
     to @(tsee exec-block-item).
     The limit returned from this ACL2 function is the latter,
     because it refers to @(tsee exec-block-item-list)."))
  (b* (((reterr) (irr-stmt-gout))
       ((stmt-gin gin) gin)
       (wrld (w state))
       ((erp (expr-gout expr))
        (atc-gen-expr term
                      (make-expr-gin :context gin.context
                                     :var-term-alist gin.var-term-alist
                                     :inscope gin.inscope
                                     :fn gin.fn
                                     :fn-guard gin.fn-guard
                                     :compst-var gin.compst-var
                                     :fenv-var gin.fenv-var
                                     :limit-var gin.limit-var
                                     :prec-fns gin.prec-fns
                                     :prec-tags gin.prec-tags
                                     :thm-index gin.thm-index
                                     :names-to-avoid gin.names-to-avoid
                                     :proofs gin.proofs)
                      state))
       ((unless (equal expr.affect must-affect))
        (reterr
         (msg "When generating code for the function ~x0, ~
               a term ~x1 was encountered at the end of the computation, ~
               which represents a return statement
               whose expression affects the variables ~x2, ~
               but ~@3 must be affected here instead."
              gin.fn
              term
              expr.affect
              (if (consp must-affect)
                  (if (consp (cdr must-affect))
                      (msg "the variables ~&0" must-affect)
                    (msg "the variable ~x0" (car must-affect)))
                "no variables"))))
       ((when (type-case expr.type :void))
        (reterr
         (raise "Internal error: return term ~x0 has type void." term)))
       ((when (type-case expr.type :array))
        (reterr
         (raise "Internal error: retun term ~x0 has type ~x1." expr.type)))
       ((when (type-case expr.type :pointer))
        (reterr
         (msg "When generating a return statement for function ~x0, ~
               the term ~x1 that represents the return expression ~
               has pointer type ~x2, which is disallowed."
              gin.fn term expr.type)))
       (stmt (make-stmt-return :value expr.expr))
       (item (block-item-stmt stmt))
       (items (list item))
       (stmt-limit (pseudo-term-fncall
                    'binary-+
                    (list (pseudo-term-quote 1)
                          expr.limit)))
       (item-limit (pseudo-term-fncall
                    'binary-+
                    (list (pseudo-term-quote 1)
                          stmt-limit)))
       (items-limit (pseudo-term-fncall
                     'binary-+
                     (list (pseudo-term-quote 1)
                           item-limit)))
       ((when (not expr.proofs))
        (retok (make-stmt-gout
                :items items
                :type expr.type
                :limit items-limit
                :events expr.events
                :thm-index expr.thm-index
                :names-to-avoid expr.names-to-avoid
                :proofs nil)))
       (thm-index expr.thm-index)
       (names-to-avoid expr.names-to-avoid)
       (type-pred (type-to-recognizer expr.type wrld))
       (valuep-when-type-pred (pack 'valuep-when- type-pred))
       (stmt-thm-name (pack gin.fn '-stmt- thm-index '-correct))
       (thm-index (1+ thm-index))
       ((mv stmt-thm-name names-to-avoid)
        (fresh-logical-name-with-$s-suffix
         stmt-thm-name nil names-to-avoid wrld))
       (stmt-formula `(and (equal (exec-stmt ',stmt
                                             ,gin.compst-var
                                             ,gin.fenv-var
                                             ,gin.limit-var)
                                  (mv ,term ,gin.compst-var))
                           (,type-pred ,term)))
       (stmt-formula (atc-contextualize stmt-formula gin.context))
       (stmt-formula `(implies (and (compustatep ,gin.compst-var)
                                    (,gin.fn-guard ,@(formals+ gin.fn wrld))
                                    (integerp ,gin.limit-var)
                                    (>= ,gin.limit-var ,stmt-limit))
                               ,stmt-formula))
       (stmt-hints
        `(("Goal" :in-theory '(exec-stmt-when-return
                               (:e stmt-kind)
                               not-zp-of-limit-variable
                               (:e stmt-return->value)
                               mv-nth-of-cons
                               (:e zp)
                               ,valuep-when-type-pred
                               ,expr.thm-name))))
       ((mv stmt-event &) (evmac-generate-defthm stmt-thm-name
                                                 :formula stmt-formula
                                                 :hints stmt-hints
                                                 :enable nil))
       (item-thm-name (pack gin.fn '-blockitem- thm-index '-correct))
       (thm-index (1+ thm-index))
       ((mv item-thm-name names-to-avoid)
        (fresh-logical-name-with-$s-suffix
         item-thm-name nil names-to-avoid wrld))
       (item-formula `(and (equal (exec-block-item ',item
                                                   ,gin.compst-var
                                                   ,gin.fenv-var
                                                   ,gin.limit-var)
                                  (mv ,term ,gin.compst-var))
                           (,type-pred ,term)))
       (item-formula (atc-contextualize item-formula gin.context))
       (item-formula `(implies (and (compustatep ,gin.compst-var)
                                    (,gin.fn-guard ,@(formals+ gin.fn wrld))
                                    (integerp ,gin.limit-var)
                                    (>= ,gin.limit-var ,item-limit))
                               ,item-formula))
       (item-hints
        `(("Goal" :in-theory '(exec-block-item-when-stmt
                               (:e block-item-kind)
                               not-zp-of-limit-variable
                               (:e block-item-stmt->get)
                               ,stmt-thm-name))))
       ((mv item-event &) (evmac-generate-defthm item-thm-name
                                                 :formula item-formula
                                                 :hints item-hints
                                                 :enable nil))
       (items-thm-name (pack gin.fn '-blockitems- thm-index '-correct))
       (thm-index (1+ thm-index))
       ((mv items-thm-name names-to-avoid)
        (fresh-logical-name-with-$s-suffix
         items-thm-name nil names-to-avoid wrld))
       (items-formula `(and (equal (exec-block-item-list ',items
                                                         ,gin.compst-var
                                                         ,gin.fenv-var
                                                         ,gin.limit-var)
                                   (mv ,term ,gin.compst-var))
                            (,type-pred ,term)))
       (items-formula (atc-contextualize items-formula gin.context))
       (items-formula `(implies (and (compustatep ,gin.compst-var)
                                     (,gin.fn-guard ,@(formals+ gin.fn wrld))
                                     (integerp ,gin.limit-var)
                                     (>= ,gin.limit-var ,items-limit))
                                ,items-formula))
       (items-hints
        `(("Goal" :in-theory '(exec-block-item-list-when-consp
                               not-zp-of-limit-variable
                               mv-nth-of-cons
                               (:e zp)
                               value-optionp-when-valuep
                               ,valuep-when-type-pred
                               ,item-thm-name
                               exec-block-item-list-of-nil
                               not-zp-of-limit-minus-const))))
       ((mv items-event &) (evmac-generate-defthm items-thm-name
                                                  :formula items-formula
                                                  :hints items-hints
                                                  :enable nil)))
    (retok (make-stmt-gout :items items
                           :type expr.type
                           :limit items-limit
                           :events (append expr.events
                                           (list stmt-event)
                                           (list item-event)
                                           (list items-event))
                           :thm-name items-thm-name
                           :thm-index thm-index
                           :names-to-avoid names-to-avoid
                           :proofs t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atc-gen-stmt ((term pseudo-termp) (gin stmt-ginp) state)
  :returns (mv erp (gout stmt-goutp))
  :short "Generate a C statement from an ACL2 term."
  :long
  (xdoc::topstring
   (xdoc::p
    "More precisely, we return a list of block items.
     These can be regarded as forming a compound statement,
     but lists of block items are compositional (via concatenation).")
   (xdoc::p
    "At the same time, we check that the term is a statement term,
     as described in the user documentation.")
   (xdoc::p
    "Along with the term, we pass an alist from symbols to terms
     that collects the @(tsee let) and @(tsee mv-let) bindings
     encountered along the way.
     These are eventually used to properly instantiate
     limits associated to function calls,
     because those limits apply to the functions' formals,
     which must therefore be replaced not just with the actuals of the call,
     but with those actuals with variables replaced with terms
     according to the bindings that lead to the call.")
   (xdoc::p
    "The @('loop-flag') input of this ACL2 function (see @(tsee stmt-gin))
     is the loop flag @('L') described in the user documentation.")
   (xdoc::p
    "The @('affect') input of this ACL2 function (see @(tsee stmt-gin))
     is the list of variables being affected by this statement.
     This is denoted @('vars') in the user documentation at @(tsee atc).")
   (xdoc::p
    "Besides the generated block items,
     we also return a C type, which is the one returned by the statement.
     This type may be @('void').")
   (xdoc::p
    "We also return a limit that suffices for @(tsee exec-block-item-list)
     to execute the returned block items completely.")
   (xdoc::p
    "We also return the correctness theorems for expressions
     generated for the expressions contained in the generated statement.")
   (xdoc::p
    "If the term is a conditional, there are two cases.
     If the test is @(tsee mbt) or @(tsee mbt$),
     we discard test and `else' branch
     and recursively translate the `then' branch;
     the limit is the same as the `then' branch.
     Otherwise, we generate an @('if') statement
     (as a singleton block item list),
     with recursively generated compound statements as branches;
     the test expression is generated from the test term;
     we ensure that the two branches have the same type.
     When we process the branches,
     we extend the symbol table with a new empty scope for each branch.
     The calculation of the limit result is a bit more complicated in this case:
     we need 1 to go from @(tsee exec-block-item-list)
     to @(tsee exec-block-item),
     another 1 to go from that to @(tsee exec-stmt),
     and another 1 to go to the @(':ifelse') case there;
     the test is pure and so it needs no addition to the limit;
     since either branch may be taken,
     we return the sum of the limits for the two branches.
     More precisely, the limit recursively returned for each branch
     pertains to the block item list in the branch,
     but those are put into a compound statement;
     thus, we need to increase the recursively calculated limit
     by 1 to go from @(tsee exec-block-item-list) to @(tsee exec-block-item),
     and another 1 to go from there to @(tsee exec-stmt).
     In principle we could return the maximum from the two branches
     instead of their sum,
     but we want the limits to be
     linear combinations of sub-limits,
     so that ACL2's linear arithmetic can handle the reasoning about limits
     during the generated proofs.")
   (xdoc::p
    "If the term is a @(tsee mv-let),
     there are three cases.
     If the term involves a @('declar<n>') wrapper,
     we ensure that a variable with
     the same symbol name as the first bound variable
     is not already in scope
     (i.e. in the symbol table)
     and that the name is a portable ASCII identifier;
     we generate a declaration for the variable,
     initialized with the expression obtained
     from the term that the variable is bound to,
     which also determines the type of the variable,
     and which must affect the bound variables except the first one;
     the type must not be a pointer type (code generation fails if it is);
     we also ensure that the other variables are assignable.
     Otherwise, if the term involves an @('assign<n>') wrapper,
     we ensure that the first bound variable is assignable,
     which implies that it must be in scope,
     and we also ensure that it has the same type as the one in scope;
     we generate an assignment whose right-hand side is
     obtained from the unwrapped term,
     which must be an expression term returning a C value
     that affects the bound variables except the first one;
     we also ensure that the other variables are assignable.
     Otherwise, if the term involves no wrapper,
     we ensure that the bound variables are all assignable,
     and that the non-wrapped term has the form
     described in the user documentation;
     we generate code that affects the variables from that term.
     In all cases, we recursively generate the block items for the body
     and we put those just after the preceding code.
     We use the sum of the two limits as the overall limit:
     thus, after @(tsee exec-block-item-list) executes
     the block items for the bound term,
     it still has enough limit to execute the block items for the body term.")
   (xdoc::p
    "If the term is a @(tsee let), there are six cases.
     If the binding has the form of an array write,
     we generate an array assignment.
     If the binding has the form of a structure scalar member write,
     we generate an assignment to
     the member of the structure,
     by value or by pointer
     If the binding has the form of a structure array member write,
     we generate an assignment to
     the element of the member of the structure,
     by value or by pointer.
     The other three cases are similar to
     the three @(tsee mv-let) cases above.
     The limit is calculated as follows.
     For the case of the term representing code that affects variables,
     we add up the two limits,
     similarly to the @(tsee mv-let) case.
     For the other cases, we have one block item followed by block items.
     First, we need 1 to go from @(tsee exec-block-item-list)
     to @(tsee exec-block-item).
     Then we take the sum of the limit for the first block item
     and the limit for the remaining block items
     (in principle we could take the maximum,
     but see the discussion above for @(tsee if)
     for why we take the sum instead).
     The first block item is a declaration, an assignment, or a function call.
     If it is a declaration, we need 1 to go from @(tsee exec-block-item)
     to the @(':declon') case and to @(tsee exec-expr-call-or-pure),
     for which we get the limit.
     If it is an assignment, we need 1 to go from @(tsee exec-block-item)
     to the @(':stmt') case and to @(tsee exec-stmt),
     another 1 to go from there to the @(':expr') case
     and to @(tsee exec-expr-call-or-asg),
     another 1 to fo from there to @(tsee exec-expr-asg),
     and another 1 to go from there to @(tsee exec-expr-call-or-pure),
     for which we recursively get the limit.
     For the remaining block items, we need to add another 1
     to go from @(tsee exec-block-item-list) to its recursive call.")
   (xdoc::p
    "If the term is a single variable
     and @('affect') is a singleton list with that variable,
     there are two cases:
     if the loop flag is @('t'), it is an error;
     otherwise, we return nothing, because
     this is the end of a list of block items that affects that variable.
     We generate 1 as the limit,
     because we need 1 to go from @(tsee exec-block-item-list)
     to the empty list case.")
   (xdoc::p
    "If the term is an @(tsee mv), there are three cases.
     If the loop flag is @('t'), it is an error.
     Otherwise, if the arguments of @(tsee mv) are the @('affect') variables,
     we return nothing, because
     this is the end of a list of block items that affects that variable;
     we return 1 as the limit, for the same reason as the case above.
     Otherwise, if the @(tsee cdr) of the arguments of @(tsee mv)
     are the @('affect') variables,
     we treat the @(tsee car) of the arguments of @(tsee mv)
     as an expression term that must affect no variables,
     and generate a return statement for it.")
   (xdoc::p
    "If the term is a call of a recursive target function on its formals,
     different from the current function @('fn'),
     then the term represents a loop.
     The loop flag must be @('nil') for this to be allowed.
     We retrieve the associated loop statement and return it.
     We also retrieve the associated limit term,
     which, as explained in @(tsee atc-fn-info),
     suffices to execute @(tsee exec-stmt-while).
     But here we are executing lists of block items,
     so we need to add 1 to go from @(tsee exec-block-item-list)
     to the call to @(tsee exec-block-item),
     another 1 to go from there to the call to @(tsee exec-stmt),
     and another 1 to go from there to the call to @(tsee exec-stmt-while).")
   (xdoc::p
    "If the term is a call of the current function @('fn') on its formals,
     we ensure that the loop flag is @('t'),
     and we generate no code.
     This represents the conclusion of a loop body (on some path).")
   (xdoc::p
    "If the term is a call of
     a non-recursive target function that returns @('void'),
     the term represents an expression statement
     consisting of a call to the corresponding C function.
     The loop flag must be @('nil') for this to be allowed.
     We ensure that all the pointer arguments are equal to the formals,
     and that the variables affected by the called function are correct.
     We retrieve the limit term associated to the called function,
     which, as explained in @(tsee atc-fn-info),
     suffices to execute @(tsee exec-fun).
     But here we are executing lists of block items,
     so we need to add 1 to go from @(tsee exec-block-item-list)
     to the call of @(tsee exec-block-item),
     another 1 to go from there to the call of @(tsee exec-stmt),
     another 1 to go from there to the call of @(tsee exec-expr-call-or-asg),
     another 1 to go from there to the call of @(tsee exec-expr-call),
     and another 1 to go from there to the call of @(tsee exec-fun).")
   (xdoc::p
    "If the term does not have any of the forms above,
     we treat it as an expression term returning a C value.
     We ensure that the loop flag is @('nil').
     We also ensure that the expression affects
     the same variables as the statement term.
     For the limit, we need 1 to go from @(tsee exec-block-item-list)
     to @(tsee exec-block-item),
     another 1 to go from there to the @(':stmt') case and @(tsee exec-stmt),
     another 1 to go from there to the @(':return') case
     and @(tsee exec-expr-call-or-pure),
     for which we use the recursively calculated limit."))
  (b* (((reterr) (irr-stmt-gout))
       (wrld (w state))
       ((stmt-gin gin) gin)
       ((mv okp test-term then-term else-term) (fty-check-if-call term))
       ((when okp)
        (b* (((mv mbtp &) (check-mbt-call test-term))
             ((when mbtp)
              (b* (((erp out) (atc-gen-stmt then-term gin state)))
                (retok (change-stmt-gout out :proofs nil))))
             ((mv mbt$p &) (check-mbt$-call test-term))
             ((when mbt$p)
              (b* (((erp out) (atc-gen-stmt then-term gin state)))
                (retok (change-stmt-gout out :proofs nil))))
             ((erp (bexpr-gout test))
              (atc-gen-expr-bool test-term
                                 (make-bexpr-gin
                                  :context gin.context
                                  :inscope gin.inscope
                                  :prec-tags gin.prec-tags
                                  :fn gin.fn
                                  :fn-guard gin.fn-guard
                                  :compst-var gin.compst-var
                                  :thm-index gin.thm-index
                                  :names-to-avoid gin.names-to-avoid
                                  :proofs gin.proofs)
                                 state))
             ((erp (stmt-gout then))
              (atc-gen-stmt then-term
                            (change-stmt-gin
                             gin
                             :inscope (cons nil gin.inscope)
                             :thm-index test.thm-index
                             :names-to-avoid test.names-to-avoid
                             :proofs nil)
                            state))
             ((erp (stmt-gout else))
              (atc-gen-stmt else-term
                            (change-stmt-gin
                             gin
                             :inscope (cons nil gin.inscope)
                             :thm-index then.thm-index
                             :names-to-avoid then.names-to-avoid
                             :proofs nil)
                            state))
             ((unless (equal then.type else.type))
              (reterr
               (msg "When generating C code for the function ~x0, ~
                     two branches ~x1 and ~x2 of a conditional term ~
                     have different types ~x3 and ~x4; ~
                     use conversion operations, if needed, ~
                     to make the branches of the same type."
                    gin.fn then-term else-term then.type else.type)))
             (type then.type)
             (limit (pseudo-term-fncall
                     'binary-+
                     (list
                      (pseudo-term-quote 5)
                      (pseudo-term-fncall
                       'binary-+
                       (list then.limit else.limit))))))
          (retok
           (make-stmt-gout
            :items
            (list
             (block-item-stmt
              (make-stmt-ifelse :test test.expr
                                :then (make-stmt-compound :items then.items)
                                :else (make-stmt-compound :items else.items))))
            :type type
            :limit limit
            :events (append test.events then.events else.events)
            :thm-name nil
            :thm-index else.thm-index
            :names-to-avoid else.names-to-avoid
            :proofs nil))))
       ((mv okp var? vars indices val-term body-term wrapper?)
        (atc-check-mv-let term))
       ((when okp)
        (b* ((all-vars (if var? (cons var? vars) vars))
             (val-instance (fty-fsublis-var gin.var-term-alist val-term))
             (vals (atc-make-mv-nth-terms indices val-instance))
             (var-term-alist-body
              (atc-update-var-term-alist all-vars vals gin.var-term-alist))
             ((when (eq wrapper? 'declar))
              (b* ((var var?)
                   ((mv info? & errorp) (atc-check-var var gin.inscope))
                   ((when errorp)
                    (reterr
                     (msg "When generating C code for the function ~x0, ~
                           a new variable ~x1 has been encountered ~
                           that has the same symbol name as, ~
                           but different package name from, ~
                           a variable already in scope. ~
                           This is disallowed."
                          gin.fn var)))
                   ((when info?)
                    (reterr
                     (msg "The variable ~x0 in the function ~x1 ~
                           is already in scope and cannot be re-declared."
                          var gin.fn)))
                   ((unless (paident-stringp (symbol-name var)))
                    (reterr
                     (msg "The symbol name ~s0 of ~
                           the MV-LET variable ~x1 of the function ~x2 ~
                           must be a portable ASCII C identifier, ~
                           but it is not."
                          (symbol-name var) var gin.fn)))
                   ((mv info?-list innermostp-list)
                    (atc-get-vars-check-innermost vars gin.inscope))
                   ((when (member-eq nil info?-list))
                    (reterr
                     (msg "When generating C code for the function ~x0, ~
                           an attempt is made to modify the variables ~x1, ~
                           not all of which are in scope."
                          gin.fn vars)))
                   ((unless (atc-vars-assignablep
                             vars innermostp-list gin.affect))
                    (reterr
                     (msg "When generating C code for the function ~x0, ~
                           an attempt is made to modify the variables ~x1, ~
                           not all of which are assignable."
                          gin.fn vars)))
                   ((erp (expr-gout init))
                    (atc-gen-expr val-term
                                  (make-expr-gin
                                   :context gin.context
                                   :var-term-alist gin.var-term-alist
                                   :inscope gin.inscope
                                   :fn gin.fn
                                   :fn-guard gin.fn-guard
                                   :compst-var gin.compst-var
                                   :fenv-var gin.fenv-var
                                   :limit-var gin.limit-var
                                   :prec-fns gin.prec-fns
                                   :prec-tags gin.prec-tags
                                   :thm-index gin.thm-index
                                   :names-to-avoid gin.names-to-avoid
                                   :proofs gin.proofs)
                                  state))
                   ((when (type-case init.type :pointer))
                    (reterr
                     (msg "When generating C code for the function ~x0, ~
                           the term ~x1 of pointer type ~x2 ~
                           is being assigned to a new variable ~x3. ~
                           This is currently disallowed, ~
                           because it would create an alias."
                          gin.fn val-term init.type var)))
                   ((unless (equal init.affect vars))
                    (reterr
                     (msg "The term ~x0 to which the variable ~x1 is bound ~
                           must affect the variables ~x2, ~
                           but it affects ~x3 instead."
                          val-term var vars init.affect)))
                   ((erp)
                    (atc-ensure-formals-not-lost vars
                                                 gin.affect
                                                 gin.typed-formals
                                                 gin.fn
                                                 wrld))
                   ((mv tyspec declor) (ident+type-to-tyspec+declor
                                        (make-ident :name (symbol-name var))
                                        init.type))
                   (declon (make-obj-declon :scspec (scspecseq-none)
                                            :tyspec tyspec
                                            :declor declor
                                            :init? (initer-single init.expr)))
                   (item (block-item-declon declon))
                   (varinfo (make-atc-var-info :type init.type :thm nil))
                   (inscope-body (atc-add-var var varinfo gin.inscope))
                   ((erp (stmt-gout body))
                    (atc-gen-stmt body-term
                                  (change-stmt-gin
                                   gin
                                   :var-term-alist var-term-alist-body
                                   :inscope inscope-body
                                   :thm-index init.thm-index
                                   :names-to-avoid init.names-to-avoid
                                   :proofs nil)
                                  state))
                   (type body.type)
                   (limit (pseudo-term-fncall
                           'binary-+
                           (list (pseudo-term-quote 3)
                                 (pseudo-term-fncall
                                  'binary-+
                                  (list init.limit body.limit))))))
                (retok (make-stmt-gout
                        :items (cons item body.items)
                        :type type
                        :limit limit
                        :events (append init.events body.events)
                        :thm-name nil
                        :thm-index body.thm-index
                        :names-to-avoid body.names-to-avoid
                        :proofs nil))))
             ((when (eq wrapper? 'assign))
              (b* ((var var?)
                   ((mv info? innermostp &) (atc-check-var var gin.inscope))
                   ((unless info?)
                    (reterr
                     (msg "When generating C code for the function ~x0, ~
                           an attempt is being made ~
                           to modify a variable ~x1 not in scope."
                          gin.fn var)))
                   ((unless (atc-var-assignablep var innermostp gin.affect))
                    (reterr
                     (msg "When generating C code for the function ~x0, ~
                           an attempt is being made ~
                           to modify a non-assignable variable ~x1."
                          gin.fn var)))
                   (prev-type (atc-var-info->type info?))
                   ((erp (expr-gout rhs))
                    (atc-gen-expr val-term
                                  (make-expr-gin
                                   :context gin.context
                                   :var-term-alist gin.var-term-alist
                                   :inscope gin.inscope
                                   :fn gin.fn
                                   :fn-guard gin.fn-guard
                                   :compst-var gin.compst-var
                                   :fenv-var gin.fenv-var
                                   :limit-var gin.limit-var
                                   :prec-fns gin.prec-fns
                                   :prec-tags gin.prec-tags
                                   :thm-index gin.thm-index
                                   :names-to-avoid gin.names-to-avoid
                                   :proofs gin.proofs)
                                  state))
                   ((unless (equal prev-type rhs.type))
                    (reterr
                     (msg "The type ~x0 of the term ~x1 ~
                           assigned to the LET variable ~x2 ~
                           of the function ~x3 ~
                           differs from the type ~x4 ~
                           of a variable with the same symbol in scope."
                          rhs.type val-term var gin.fn prev-type)))
                   ((unless (equal rhs.affect vars))
                    (reterr
                     (msg "The term ~x0 to which the variable ~x1 is bound ~
                           must affect the variables ~x2, ~
                           but it affects ~x3 instead."
                          val-term var vars rhs.affect)))
                   ((erp)
                    (atc-ensure-formals-not-lost vars
                                                 gin.affect
                                                 gin.typed-formals
                                                 gin.fn
                                                 wrld))
                   ((when (type-case rhs.type :array))
                    (reterr (raise "Internal error: array type ~x0." rhs.type)))
                   ((when (type-case rhs.type :pointer))
                    (reterr
                     (msg "The term ~x0 to which the variable ~x1 is bound ~
                           must not have a C pointer type, ~
                           but it has type ~x2 instead."
                          val-term var rhs.type)))
                   (asg (make-expr-binary
                         :op (binop-asg)
                         :arg1 (expr-ident (make-ident :name (symbol-name var)))
                         :arg2 rhs.expr))
                   (stmt (stmt-expr asg))
                   (item (block-item-stmt stmt))
                   ((erp (stmt-gout body))
                    (atc-gen-stmt body-term
                                  (change-stmt-gin
                                   gin
                                   :var-term-alist var-term-alist-body
                                   :thm-index rhs.thm-index
                                   :names-to-avoid rhs.names-to-avoid
                                   :proofs nil)
                                  state))
                   (type body.type)
                   (limit (pseudo-term-fncall
                           'binary-+
                           (list (pseudo-term-quote 6)
                                 (pseudo-term-fncall
                                  'binary-+
                                  (list rhs.limit body.limit))))))
                (retok (make-stmt-gout
                        :items (cons item body.items)
                        :type type
                        :limit limit
                        :events (append rhs.events body.events)
                        :thm-name nil
                        :thm-index body.thm-index
                        :names-to-avoid body.names-to-avoid
                        :proofs nil))))
             ((unless (eq wrapper? nil))
              (reterr
               (raise "Internal error: MV-LET wrapper is ~x0." wrapper?)))
             ((mv info?-list innermostp-list)
              (atc-get-vars-check-innermost vars gin.inscope))
             ((when (member-eq nil info?-list))
              (reterr
               (msg "When generating C code for the function ~x0, ~
                     an attempt is made to modify the variables ~x1, ~
                     not all of which are in scope."
                    gin.fn vars)))
             ((unless (atc-vars-assignablep vars innermostp-list gin.affect))
              (reterr
               (msg "When generating C code for the function ~x0, ~
                     an attempt is made to modify the variables ~x1, ~
                     not all of which are assignable."
                    gin.fn vars)))
             ((unless (atc-affecting-term-for-let-p val-term gin.prec-fns))
              (reterr
               (msg "When generating C code for the function ~x0, ~
                     an MV-LET has been encountered ~
                     whose term ~x1 to which the variables are bound ~
                     does not have the required form."
                    gin.fn val-term)))
             ((erp)
              (atc-ensure-formals-not-lost vars
                                           gin.affect
                                           gin.typed-formals
                                           gin.fn
                                           wrld))
             ((erp (stmt-gout xform))
              (atc-gen-stmt val-term
                            (change-stmt-gin gin
                                             :affect vars
                                             :loop-flag nil)
                            state))
             ((unless (type-case xform.type :void))
              (reterr
               (msg "When generating C code for the function ~x0, ~
                     an MV-LET has been encountered ~
                     whose term ~x1 to which the variables are bound ~
                     has the non-void type ~x2, ~
                     which is disallowed."
                    gin.fn val-term xform.type)))
             ((erp (stmt-gout body))
              (atc-gen-stmt body-term
                            (change-stmt-gin
                             gin
                             :var-term-alist var-term-alist-body
                             :thm-index xform.thm-index
                             :names-to-avoid xform.names-to-avoid
                             :proofs nil)
                            state))
             (items (append xform.items body.items))
             (type body.type)
             (limit (pseudo-term-fncall 'binary-+
                                        (list xform.limit body.limit))))
          (retok (make-stmt-gout
                  :items items
                  :type type
                  :limit limit
                  :events (append xform.events body.events)
                  :thm-name nil
                  :thm-index body.thm-index
                  :names-to-avoid body.names-to-avoid
                  :proofs nil))))
       ((mv okp var val-term body-term wrapper?) (atc-check-let term))
       ((when okp)
        (b* ((val-instance (fty-fsublis-var gin.var-term-alist val-term))
             (var-term-alist-body
              (atc-update-var-term-alist (list var)
                                         (list val-instance)
                                         gin.var-term-alist))
             ((mv okp sub-term elem-term sub-type elem-type)
              (atc-check-array-write var val-term))
             ((when okp)
              (b* (((unless (eq wrapper? nil))
                    (reterr
                     (msg "The array write term ~x0 to which ~x1 is bound ~
                           has the ~x2 wrapper, which is disallowed."
                          val-term var wrapper?)))
                   ((unless (member-eq var gin.affect))
                    (reterr
                     (msg "The array ~x0 is being written to, ~
                           but it is not among the variables ~x1 ~
                           currently affected."
                          var gin.affect)))
                   ((erp (pexpr-gout arr))
                    (atc-gen-expr-pure var
                                       (make-pexpr-gin
                                        :context gin.context
                                        :inscope gin.inscope
                                        :prec-tags gin.prec-tags
                                        :fn gin.fn
                                        :fn-guard gin.fn-guard
                                        :compst-var gin.compst-var
                                        :thm-index gin.thm-index
                                        :names-to-avoid gin.names-to-avoid
                                        :proofs gin.proofs)
                                       state))
                   ((erp (pexpr-gout sub))
                    (atc-gen-expr-pure sub-term
                                       (make-pexpr-gin
                                        :context gin.context
                                        :inscope gin.inscope
                                        :prec-tags gin.prec-tags
                                        :fn gin.fn
                                        :fn-guard gin.fn-guard
                                        :compst-var gin.compst-var
                                        :thm-index arr.thm-index
                                        :names-to-avoid arr.names-to-avoid
                                        :proofs arr.proofs)
                                       state))
                   ((erp (pexpr-gout elem))
                    (atc-gen-expr-pure elem-term
                                       (make-pexpr-gin
                                        :context gin.context
                                        :inscope gin.inscope
                                        :prec-tags gin.prec-tags
                                        :fn gin.fn
                                        :fn-guard gin.fn-guard
                                        :compst-var gin.compst-var
                                        :thm-index sub.thm-index
                                        :names-to-avoid sub.names-to-avoid
                                        :proofs sub.proofs)
                                       state))
                   ((unless (equal arr.type (type-pointer elem-type)))
                    (reterr
                     (msg "The array ~x0 of type ~x1 ~
                           does not have the expected type ~x2. ~
                           This is indicative of ~
                           unreachable code under the guards, ~
                           given that the code is guard-verified."
                          var arr.type (type-pointer elem-type))))
                   ((unless (equal sub.type sub-type))
                    (reterr
                     (msg "The array ~x0 of type ~x1 ~
                           is being indexed with ~
                           a subscript ~x2 of type x3, ~
                           instead of type ~x4 as expected.
                           This is indicative of ~
                           unreachable code under the guards, ~
                           given that the code is guard-verified."
                          var arr.type sub sub.type sub-type)))
                   ((unless (equal elem.type elem-type))
                    (reterr
                     (msg "The array ~x0 of type ~x1 ~
                           is being written to with ~
                           an element ~x2 of type x3, ~
                           instead of type ~x4 as expected.
                           This is indicative of ~
                           unreachable code under the guards, ~
                           given that the code is guard-verified."
                          var arr.type elem elem.type elem-type)))
                   (asg (make-expr-binary
                         :op (binop-asg)
                         :arg1 (make-expr-arrsub :arr arr.expr
                                                 :sub sub.expr)
                         :arg2 elem.expr))
                   (stmt (stmt-expr asg))
                   (item (block-item-stmt stmt))
                   ((erp (stmt-gout body))
                    (atc-gen-stmt body-term
                                  (change-stmt-gin
                                   gin
                                   :var-term-alist var-term-alist-body
                                   :thm-index elem.thm-index
                                   :names-to-avoid elem.names-to-avoid
                                   :proofs nil)
                                  state))
                   (limit (pseudo-term-fncall 'binary-+
                                              (list (pseudo-term-quote 4)
                                                    body.limit))))
                (retok (make-stmt-gout
                        :items (cons item body.items)
                        :type body.type
                        :limit limit
                        :events (append arr.events
                                        sub.events
                                        elem.events
                                        body.events)
                        :thm-name nil
                        :thm-index body.thm-index
                        :names-to-avoid body.names-to-avoid
                        :proofs nil))))
             ((mv okp member-term tag member-name member-type)
              (atc-check-struct-write-scalar var val-term gin.prec-tags))
             ((when okp)
              (b* (((unless (eq wrapper? nil))
                    (reterr
                     (msg "The structure write term ~x0 ~
                           to which ~x1 is bound ~
                           has the ~x2 wrapper, which is disallowed."
                          val-term var wrapper?)))
                   ((erp (pexpr-gout struct))
                    (atc-gen-expr-pure var
                                       (make-pexpr-gin
                                        :context gin.context
                                        :inscope gin.inscope
                                        :prec-tags gin.prec-tags
                                        :fn gin.fn
                                        :fn-guard gin.fn-guard
                                        :compst-var gin.compst-var
                                        :thm-index gin.thm-index
                                        :names-to-avoid gin.names-to-avoid
                                        :proofs gin.proofs)
                                       state))
                   ((erp pointerp)
                    (cond
                     ((equal struct.type (type-struct tag))
                      (retok nil))
                     ((equal struct.type (type-pointer (type-struct tag)))
                      (retok t))
                     (t (reterr
                         (msg "The structure ~x0 of type ~x1 ~
                               does not have the expected type ~x2 or ~x3. ~
                               This is indicative of ~
                               unreachable code under the guards, ~
                               given that the code is guard-verified."
                              var
                              struct.type
                              (type-struct tag)
                              (type-pointer (type-struct tag)))))))
                   ((when (and pointerp
                               (not (member-eq var gin.affect))))
                    (reterr
                     (msg "The structure ~x0 ~
                           is being written to by pointer, ~
                           but it is not among the variables ~x1 ~
                           currently affected."
                          var gin.affect)))
                   ((erp (pexpr-gout member))
                    (atc-gen-expr-pure member-term
                                       (make-pexpr-gin
                                        :context gin.context
                                        :inscope gin.inscope
                                        :prec-tags gin.prec-tags
                                        :fn gin.fn
                                        :fn-guard gin.fn-guard
                                        :compst-var gin.compst-var
                                        :thm-index struct.thm-index
                                        :names-to-avoid struct.names-to-avoid
                                        :proofs struct.proofs)
                                       state))
                   ((unless (equal member.type member-type))
                    (reterr
                     (msg "The structure ~x0 of type ~x1 ~
                           is being written to with ~
                           a member ~x2 of type ~x3, ~
                           instead of type ~x4 as expected. ~
                           This is indicative of ~
                           unreachable code under the guards, ~
                           given that the code is guard-verified."
                          var struct.type member-term
                          member.type member-type)))
                   (asg-mem (if pointerp
                                (make-expr-memberp :target struct.expr
                                                   :name member-name)
                              (make-expr-member :target struct.expr
                                                :name member-name)))
                   (asg (make-expr-binary :op (binop-asg)
                                          :arg1 asg-mem
                                          :arg2 member.expr))
                   (stmt (stmt-expr asg))
                   (item (block-item-stmt stmt))
                   ((erp (stmt-gout body))
                    (atc-gen-stmt body-term
                                  (change-stmt-gin
                                   gin
                                   :var-term-alist var-term-alist-body
                                   :thm-index member.thm-index
                                   :names-to-avoid member.names-to-avoid
                                   :proofs nil)
                                  state))
                   (limit (pseudo-term-fncall 'binary-+
                                              (list (pseudo-term-quote 4)
                                                    body.limit))))
                (retok (make-stmt-gout
                        :items (cons item body.items)
                        :type body.type
                        :limit limit
                        :events (append struct.events
                                        member.events)
                        :thm-name nil
                        :thm-index body.thm-index
                        :names-to-avoid body.names-to-avoid
                        :proofs nil))))
             ((mv okp index-term elem-term tag member index-type elem-type)
              (atc-check-struct-write-array var val-term gin.prec-tags))
             ((when okp)
              (b* (((unless (eq wrapper? nil))
                    (reterr
                     (msg "The structure write term ~x0 ~
                           to which ~x1 is bound ~
                           has the ~x2 wrapper, which is disallowed."
                          val-term var wrapper?)))
                   ((erp (pexpr-gout struct))
                    (atc-gen-expr-pure var
                                       (make-pexpr-gin
                                        :context gin.context
                                        :inscope gin.inscope
                                        :prec-tags gin.prec-tags
                                        :fn gin.fn
                                        :fn-guard gin.fn-guard
                                        :compst-var gin.compst-var
                                        :thm-index gin.thm-index
                                        :names-to-avoid gin.names-to-avoid
                                        :proofs gin.proofs)
                                       state))
                   ((erp pointerp)
                    (cond
                     ((equal struct.type (type-struct tag))
                      (retok nil))
                     ((equal struct.type (type-pointer (type-struct tag)))
                      (retok t))
                     (t (reterr
                         (msg "The structure ~x0 of type ~x1 ~
                               does not have the expected type ~x2 or ~x3. ~
                               This is indicative of ~
                               unreachable code under the guards, ~
                               given that the code is guard-verified."
                              var
                              struct.type
                              (type-struct tag)
                              (type-pointer (type-struct tag)))))))
                   ((when (and pointerp
                               (not (member-eq var gin.affect))))
                    (reterr
                     (msg "The structure ~x0 ~
                           is being written to by pointer, ~
                           but it is not among the variables ~x1 ~
                           currently affected."
                          var gin.affect)))
                   ((erp (pexpr-gout index))
                    (atc-gen-expr-pure index-term
                                       (make-pexpr-gin
                                        :context gin.context
                                        :inscope gin.inscope
                                        :prec-tags gin.prec-tags
                                        :fn gin.fn
                                        :fn-guard gin.fn-guard
                                        :compst-var gin.compst-var
                                        :thm-index struct.thm-index
                                        :names-to-avoid struct.names-to-avoid
                                        :proofs struct.proofs)
                                       state))
                   ((unless (equal index.type index-type))
                    (reterr
                     (msg "The structure ~x0 of type ~x1 ~
                           is being written to with ~
                           an index ~x2 of type ~x3, ~
                           instead of type ~x4 as expected. ~
                           This is indicative of ~
                           unreachable code under the guards, ~
                           given that the code is guard-verified."
                          var struct.type index-term index.type index-type)))
                   ((erp (pexpr-gout elem))
                    (atc-gen-expr-pure elem-term
                                       (make-pexpr-gin
                                        :context gin.context
                                        :inscope gin.inscope
                                        :prec-tags gin.prec-tags
                                        :fn gin.fn
                                        :fn-guard gin.fn-guard
                                        :compst-var gin.compst-var
                                        :thm-index index.thm-index
                                        :names-to-avoid index.names-to-avoid
                                        :proofs index.proofs)
                                       state))
                   ((unless (equal elem.type elem-type))
                    (reterr
                     (msg "The structure ~x0 of type ~x1 ~
                           is being written to with ~
                           a member array element ~x2 of type ~x3, ~
                           instead of type ~x4 as expected.
                           This is indicative of ~
                           unreachable code under the guards, ~
                           given that the code is guard-verified."
                          var struct.type elem-term elem.type elem-type)))
                   (asg-mem (if pointerp
                                (make-expr-memberp :target struct.expr
                                                   :name member)
                              (make-expr-member :target struct.expr
                                                :name member)))
                   (asg (make-expr-binary
                         :op (binop-asg)
                         :arg1 (make-expr-arrsub :arr asg-mem
                                                 :sub index.expr)
                         :arg2 elem.expr))
                   (stmt (stmt-expr asg))
                   (item (block-item-stmt stmt))
                   ((erp (stmt-gout body))
                    (atc-gen-stmt body-term
                                  (change-stmt-gin
                                   gin
                                   :var-term-alist var-term-alist-body
                                   :thm-index elem.thm-index
                                   :names-to-avoid elem.names-to-avoid
                                   :proofs nil)
                                  state))
                   (limit (pseudo-term-fncall 'binary-+
                                              (list (pseudo-term-quote 4)
                                                    body.limit))))
                (retok (make-stmt-gout
                        :items (cons item body.items)
                        :type body.type
                        :limit limit
                        :events (append struct.events
                                        index.events
                                        elem.events
                                        body.events)
                        :thm-name nil
                        :thm-index body.thm-index
                        :names-to-avoid body.names-to-avoid
                        :proofs nil))))
             ((mv info? innermostp errorp) (atc-check-var var gin.inscope))
             ((when errorp)
              (reterr
               (msg "When generating C code for the function ~x0, ~
                     a new variable ~x1 has been encountered ~
                     that has the same symbol name as, ~
                     but different package name from, ~
                     a variable already in scope. ~
                     This is disallowed."
                    gin.fn var)))
             ((when (eq wrapper? 'declar))
              (b* (((when info?)
                    (reterr
                     (msg "The variable ~x0 in the function ~x1 ~
                           is already in scope and cannot be re-declared."
                          var gin.fn)))
                   ((unless (paident-stringp (symbol-name var)))
                    (reterr
                     (msg "The symbol name ~s0 of ~
                           the LET variable ~x1 of the function ~x2 ~
                           must be a portable ASCII C identifier, ~
                           but it is not."
                          (symbol-name var) var gin.fn)))
                   ((erp (expr-gout init))
                    (atc-gen-expr val-term
                                  (make-expr-gin
                                   :context gin.context
                                   :var-term-alist gin.var-term-alist
                                   :inscope gin.inscope
                                   :fn gin.fn
                                   :fn-guard gin.fn-guard
                                   :compst-var gin.compst-var
                                   :fenv-var gin.fenv-var
                                   :limit-var gin.limit-var
                                   :prec-fns gin.prec-fns
                                   :prec-tags gin.prec-tags
                                   :thm-index gin.thm-index
                                   :names-to-avoid gin.names-to-avoid
                                   :proofs gin.proofs)
                                  state))
                   ((when (type-case init.type :pointer))
                    (reterr
                     (msg "When generating C code for the function ~x0, ~
                           the term ~x1 of pointer type ~x2 ~
                           is being assigned to a new variable ~x3. ~
                           This is currently disallowed, ~
                           because it would create an alias."
                          gin.fn val-term init.type var)))
                   ((when (consp init.affect))
                    (reterr
                     (msg "The term ~x0 to which the variable ~x1 is bound ~
                           must not affect any variables, ~
                           but it affects ~x2 instead."
                          val-term var init.affect)))
                   ((mv tyspec declor) (ident+type-to-tyspec+declor
                                        (make-ident :name (symbol-name var))
                                        init.type))
                   (declon (make-obj-declon :scspec (scspecseq-none)
                                            :tyspec tyspec
                                            :declor declor
                                            :init? (initer-single init.expr)))
                   (item (block-item-declon declon))
                   (varinfo (make-atc-var-info :type init.type :thm nil))
                   (inscope-body (atc-add-var var varinfo gin.inscope))
                   ((erp (stmt-gout body))
                    (atc-gen-stmt body-term
                                  (change-stmt-gin
                                   gin
                                   :var-term-alist var-term-alist-body
                                   :inscope inscope-body
                                   :thm-index init.thm-index
                                   :names-to-avoid init.names-to-avoid
                                   :proofs nil)
                                  state))
                   (type body.type)
                   (limit (pseudo-term-fncall
                           'binary-+
                           (list (pseudo-term-quote 3)
                                 (pseudo-term-fncall
                                  'binary-+
                                  (list init.limit body.limit))))))
                (retok (make-stmt-gout
                        :items (cons item body.items)
                        :type type
                        :limit limit
                        :events (append init.events body.events)
                        :thm-name nil
                        :thm-index body.thm-index
                        :names-to-avoid body.names-to-avoid
                        :proofs nil))))
             ((unless (atc-var-assignablep var innermostp gin.affect))
              (reterr
               (msg "When generating C code for the function ~x0, ~
                     an attempt is being made ~
                     to modify a non-assignable variable ~x1."
                    gin.fn var)))
             ((when (eq wrapper? 'assign))
              (b* (((unless info?)
                    (reterr
                     (raise "Internal error: no information for variable ~x0."
                            var)))
                   (prev-type (atc-var-info->type info?))
                   ((erp (expr-gout rhs))
                    (atc-gen-expr val-term
                                  (make-expr-gin
                                   :context gin.context
                                   :var-term-alist gin.var-term-alist
                                   :inscope gin.inscope
                                   :fn gin.fn
                                   :fn-guard gin.fn-guard
                                   :compst-var gin.compst-var
                                   :fenv-var gin.fenv-var
                                   :limit-var gin.limit-var
                                   :prec-fns gin.prec-fns
                                   :prec-tags gin.prec-tags
                                   :thm-index gin.thm-index
                                   :names-to-avoid gin.names-to-avoid
                                   :proofs gin.proofs)
                                  state))
                   ((unless (equal prev-type rhs.type))
                    (reterr
                     (msg "The type ~x0 of the term ~x1 ~
                           assigned to the LET variable ~x2 ~
                           of the function ~x3 ~
                           differs from the type ~x4 ~
                           of a variable with the same symbol in scope."
                          rhs.type val-term var gin.fn prev-type)))
                   ((when (consp rhs.affect))
                    (reterr
                     (msg "The term ~x0 to which the variable ~x1 is bound ~
                           must not affect any variables, ~
                           but it affects ~x2 instead."
                          val-term var rhs.affect)))
                   ((when (type-case rhs.type :array))
                    (reterr
                     (raise "Internal error: array type ~x0." rhs.type)))
                   ((when (type-case rhs.type :pointer))
                    (reterr
                     (msg "The term ~x0 to which the variable ~x1 is bound ~
                           must not have a C pointer type, ~
                           but it has type ~x2 instead."
                          val-term var rhs.type)))
                   (asg (make-expr-binary
                         :op (binop-asg)
                         :arg1 (expr-ident (make-ident :name (symbol-name var)))
                         :arg2 rhs.expr))
                   (stmt (stmt-expr asg))
                   (item (block-item-stmt stmt))
                   ((erp (stmt-gout body))
                    (atc-gen-stmt body-term
                                  (change-stmt-gin
                                   gin
                                   :var-term-alist var-term-alist-body
                                   :thm-index rhs.thm-index
                                   :names-to-avoid rhs.names-to-avoid
                                   :proofs nil)
                                  state))
                   (type body.type)
                   (limit (pseudo-term-fncall
                           'binary-+
                           (list (pseudo-term-quote 6)
                                 (pseudo-term-fncall
                                  'binary-+
                                  (list rhs.limit body.limit))))))
                (retok (make-stmt-gout
                        :items (cons item body.items)
                        :type type
                        :limit limit
                        :events (append rhs.events body.events)
                        :thm-name nil
                        :thm-index body.thm-index
                        :names-to-avoid body.names-to-avoid
                        :proofs nil))))
             ((unless (eq wrapper? nil))
              (reterr (raise "Internal error: LET wrapper is ~x0." wrapper?)))
             ((unless (atc-affecting-term-for-let-p val-term gin.prec-fns))
              (reterr
               (msg "When generating C code for the function ~x0, ~
                     we encountered a LET binding ~
                     of the variable ~x1 to the term ~x2 ~
                     that does not have any of the allowed forms. ~
                     See the user documentation."
                    gin.fn var val-term)))
             ((erp)
              (atc-ensure-formals-not-lost (list var)
                                           gin.affect
                                           gin.typed-formals
                                           gin.fn
                                           wrld))
             ((erp (stmt-gout xform))
              (atc-gen-stmt val-term
                            (change-stmt-gin gin
                                             :affect (list var)
                                             :loop-flag nil
                                             :proofs gin.proofs)
                            state))
             ((unless (type-case xform.type :void))
              (reterr
               (msg "When generating C code for the function ~x0, ~
                     a LET has been encountered ~
                     whose unwrapped term ~x1 ~
                     to which the variable is bound ~
                     has the non-void type ~x2, ~
                     which is disallowed."
                    gin.fn val-term xform.type)))
             ((erp (stmt-gout body))
              (atc-gen-stmt body-term
                            (change-stmt-gin
                             gin
                             :var-term-alist var-term-alist-body
                             :thm-index xform.thm-index
                             :names-to-avoid xform.names-to-avoid
                             :proofs nil)
                            state))
             (items (append xform.items body.items))
             (type body.type)
             (limit (pseudo-term-fncall
                     'binary-+
                     (list xform.limit body.limit))))
          (retok (make-stmt-gout
                  :items items
                  :type type
                  :limit limit
                  :events (append xform.events body.events)
                  :thm-name nil
                  :thm-index body.thm-index
                  :names-to-avoid body.names-to-avoid
                  :proofs nil))))
       ((when (and (pseudo-term-case term :var)
                   (equal gin.affect
                          (list (pseudo-term-var->name term)))))
        (if gin.loop-flag
            (reterr
             (msg "A loop body must end with ~
                   a recursive call on every path, ~
                   but in the function ~x0 it ends with ~x1 instead."
                  gin.fn term))
          (retok (make-stmt-gout
                  :items nil
                  :type (type-void)
                  :limit (pseudo-term-quote 1)
                  :events nil
                  :thm-name nil
                  :thm-index gin.thm-index
                  :names-to-avoid gin.names-to-avoid
                  :proofs nil))))
       ((mv okp terms) (fty-check-list-call term))
       ((when okp)
        (b* (((unless (>= (len terms) 2))
              (reterr (raise "Internal error: MV applied to ~x0." terms)))
             ((when gin.loop-flag)
              (reterr
               (msg "A loop body must end with ~
                     a recursive call on every path, ~
                     but in the function ~x0 ~
                     it ends with ~x1 instead."
                    gin.fn term))))
          (cond
           ((equal terms gin.affect)
            (retok (make-stmt-gout :items nil
                                   :type (type-void)
                                   :limit (pseudo-term-quote 1)
                                   :events nil
                                   :thm-name nil
                                   :thm-index gin.thm-index
                                   :names-to-avoid gin.names-to-avoid
                                   :proofs nil)))
           ((equal (cdr terms) gin.affect)
            (b* ((gin (change-stmt-gin gin :proofs nil)))
              (atc-gen-return-stmt (car terms) gin nil state)))
           (t (reterr
               (msg "When generating C code for the function ~x0, ~
                     a term ~x0 has been encountered, ~
                     which is disallowed."
                    gin.fn term))))))
       ((mv okp loop-fn loop-args in-types loop-affect loop-stmt loop-limit)
        (atc-check-loop-call term gin.var-term-alist gin.prec-fns))
       ((when okp)
        (b* (((when gin.loop-flag)
              (reterr
               (msg "A loop body must end with ~
                     a recursive call on every path, ~
                     but in the function ~x0 it ends with ~x1 instead."
                    gin.fn term)))
             (formals (formals+ loop-fn wrld))
             ((unless (equal formals loop-args))
              (reterr
               (msg "When generating C code for the function ~x0, ~
                     a call of the recursive function ~x1 ~
                     has been encountered ~
                     that is not on its formals, ~
                     but instead on the arguments ~x2.
                     This is disallowed; see the ATC user documentation."
                    gin.fn loop-fn loop-args)))
             ((unless (equal gin.affect loop-affect))
              (reterr
               (msg "When generating C code for the function ~x0, ~
                     a call of the recursive function ~x1 ~
                     has been encountered
                     that represents a loop affecting ~x2, ~
                     which differs from the variables ~x3 ~
                     being affected here."
                    gin.fn loop-fn loop-affect gin.affect)))
             (infos (atc-get-vars formals gin.inscope))
             ((unless (atc-var-info-listp infos))
              (reterr
               (raise "Internal error: not all formals ~x0 have information ~x1."
                      formals infos)))
             (types (atc-var-info-list->type-list infos))
             ((unless (equal types in-types))
              (reterr
               (msg "The loop function ~x0 with input types ~x1 ~
                     is applied to terms ~x2 returning ~x3. ~
                     This is indicative of dead code under the guards, ~
                     given that the code is guard-verified."
                    loop-fn in-types formals types)))
             (limit (pseudo-term-fncall
                     'binary-+
                     (list (pseudo-term-quote 3)
                           loop-limit))))
          (retok (make-stmt-gout
                  :items (list (block-item-stmt loop-stmt))
                  :type (type-void)
                  :limit limit
                  :events nil
                  :thm-name nil
                  :thm-index gin.thm-index
                  :names-to-avoid gin.names-to-avoid
                  :proofs nil))))
       ((when (equal term `(,gin.fn ,@(formals+ gin.fn wrld))))
        (if gin.loop-flag
            (retok (make-stmt-gout
                    :items nil
                    :type (type-void)
                    :limit (pseudo-term-quote 1)
                    :events nil
                    :thm-name nil
                    :thm-index gin.thm-index
                    :names-to-avoid gin.names-to-avoid
                    :proofs nil))
          (reterr
           (msg "When generating code for the recursive function ~x0, ~
                 a recursive call to the loop function occurs ~
                 not at the end of the computation on some path."
                gin.fn))))
       ((mv okp called-fn arg-terms in-types out-type fn-affect limit)
        (atc-check-cfun-call term gin.var-term-alist gin.prec-fns wrld))
       ((when (and okp
                   (type-case out-type :void)))
        (b* (((when gin.loop-flag)
              (reterr
               (msg "A loop body must end with ~
                     a recursive call on every path, ~
                     but in the function ~x0 it ends with ~x1 instead."
                    gin.fn term)))
             ((unless (atc-check-cfun-call-args (formals+ called-fn wrld)
                                                in-types
                                                arg-terms))
              (reterr
               (msg "The call ~x0 does not satisfy the restrictions ~
                     on array arguments being identical to the formals."
                    term)))
             ((unless (equal gin.affect fn-affect))
              (reterr
               (msg "When generating C code for the function ~x0, ~
                     a call of the non-recursive function ~x1 ~
                     has been encountered that affects ~x2, ~
                     which differs from the variables ~x3 ~
                     being affected here."
                    gin.fn loop-fn fn-affect gin.affect)))
             ((erp (pexprs-gout args))
              (atc-gen-expr-pure-list arg-terms
                                      (make-pexprs-gin
                                       :context gin.context
                                       :inscope gin.inscope
                                       :prec-tags gin.prec-tags
                                       :fn gin.fn
                                       :fn-guard gin.fn-guard
                                       :compst-var gin.compst-var
                                       :thm-index gin.thm-index
                                       :names-to-avoid gin.names-to-avoid
                                       :proofs gin.proofs)
                                      state))
             ((unless (equal args.types in-types))
              (reterr
               (msg "The function ~x0 with input types ~x1 is applied to ~
                     expression terms ~x2 returning ~x3. ~
                     This is indicative of provably dead code, ~
                     given that the code is guard-verified."
                    called-fn in-types arg-terms args.types)))
             (call-expr (make-expr-call :fun (make-ident
                                              :name (symbol-name called-fn))
                                        :args args.exprs)))
          (retok (make-stmt-gout
                  :items (list (block-item-stmt (stmt-expr call-expr)))
                  :type (type-void)
                  :limit `(binary-+ '5 ,limit)
                  :events args.events
                  :thm-name nil
                  :thm-index args.thm-index
                  :names-to-avoid args.names-to-avoid
                  :proofs nil))))
       ((when gin.loop-flag)
        (reterr
         (msg "A loop body must end with ~
               a recursive call on every path, ~
               but in the function ~x0 it ends with ~x1 instead."
              gin.fn term))))
    (atc-gen-return-stmt term gin gin.affect state))

  :measure (pseudo-term-count term)

  :verify-guards nil ; done below

  ///

  (defrulel pseudo-termp-when-symbolp
    (implies (symbolp x)
             (pseudo-termp x))
    :enable pseudo-termp)

  (verify-guards atc-gen-stmt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fty::defprod lstmt-gin
  :short "Inputs for @(tsee atc-gen-loop-stmt)."
  ((context atc-contextp)
   (typed-formals atc-symbol-varinfo-alist)
   (inscope atc-symbol-varinfo-alist-list)
   (fn symbol)
   (fn-guard symbol)
   (compst-var symbol)
   (fenv-var symbol)
   (limit-var symbol)
   (measure-for-fn symbol)
   (measure-formals symbol-list)
   (prec-fns atc-symbol-fninfo-alist)
   (prec-tags atc-string-taginfo-alist)
   (prec-objs atc-string-objinfo-alist)
   (thm-index pos)
   (names-to-avoid symbol-list)
   (proofs bool))
  :pred lstmt-ginp)

;;;;;;;;;;;;;;;;;;;;

(fty::defprod lstmt-gout
  :short "Outputs for @(tsee atc-gen-loop-stmt)."
  ((stmt stmtp)
   (test-term pseudo-term)
   (body-term pseudo-term)
   (affect symbol-list)
   (limit-body pseudo-term)
   (limit-all pseudo-term)
   (events pseudo-event-form-list)
   (thm-name symbol)
   (thm-index pos)
   (names-to-avoid symbol-list)
   (proofs bool))
  :pred lstmt-goutp)

;;;;;;;;;;

(defirrelevant irr-lstmt-gout
  :short "An irrelevant output for @(tsee atc-gen-loop-stmt)."
  :type lstmt-goutp
  :body (make-lstmt-gout :stmt (irr-stmt)
                         :test-term nil
                         :body-term nil
                         :affect nil
                         :limit-body nil
                         :limit-all nil
                         :events nil
                         :thm-name nil
                         :thm-index 1
                         :names-to-avoid nil
                         :proofs nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atc-gen-loop-stmt ((term pseudo-termp) (gin lstmt-ginp) state)
  :returns (mv erp (gout lstmt-goutp))
  :short "Generate a C loop statement from an ACL2 term."
  :long
  (xdoc::topstring
   (xdoc::p
    "This is called on loop terms (see user documentation).")
   (xdoc::p
    "The term must be an @(tsee if).
     If the test is an @(tsee mbt) or @(tsee mbt$),
     test and `else' branch are ignored,
     while the `then' branch is recursively processed.
     Otherwise, the test must be an expression term returning a boolean
     from which we generate the loop test;
     the `then' branch must be a statement term,
     from which we generate the loop body;
     and the `else' branch must be either a single variable
     or an @(tsee mv) call on variables,
     which must be a subset of the function's formals,
     and from those variables we determine
     the variables affected by the loop.
     The statement term in the `then' branch
     must affect the variables found in the `else' branch.
     We return
     the term that represents the loop test,
     the term that represent the loop body
     and the variables affected by the loop.
     The loop test and body are used to generate more modular theorems.")
   (xdoc::p
    "Note that we push a new scope before processing the loop body.
     This is because the loop body is a block, which opens a new scope in C.")
   (xdoc::p
    "We return a limit that suffices
     to execute @(tsee exec-stmt-while) on (the test and body of)
     the loop statement, as follows.
     We need 1 to get to executing the test,
     which is pure and so does not contribute to the overall limit.
     If the test is true, we need to add the limit to execute the body.
     After that, @(tsee exec-stmt-while) is called recursively,
     decrementing the limit:
     given that we know that the loop function terminates,
     its measure must suffice as the limit.
     The loop function decreases the measure by at least 1 (maybe more)
     at every recursive call, so the limit does not decrease any faster,
     and we will never run out of the limit before the measure runs out.
     Thus the measure is an over-approximation for the limit, which is sound.
     We also note that the measure refers to the initial call of the function,
     while here it would suffice
     to take the measure at the first recursive call,
     but taking the whole measure is simpler,
     and again it is sound to over-appoximate.
     Note that we use the measure function for @('fn')
     that is generated by ATC,
     for the reasons explained in @(tsee atc-gen-loop-measure-fn).
     Besides the @('limit-all') result,
     which is the limit for the whole loop,
     we also return @('limit-body'), which is just for the loop body;
     this is in support for more modular proofs. "))
  (b* (((reterr) (change-lstmt-gout (irr-lstmt-gout)
                                    :stmt (make-stmt-while :test (irr-expr)
                                                           :body (irr-stmt))))
       ((lstmt-gin gin) gin)
       (wrld (w state))
       ((mv okp test-term then-term else-term) (fty-check-if-call term))
       ((unless okp)
        (reterr
         (msg "When generating C loop code for the recursive function ~x0, ~
               a term ~x1 that is not an IF has been encountered."
              gin.fn term)))
       ((mv mbtp &) (check-mbt-call test-term))
       ((when mbtp) (atc-gen-loop-stmt then-term gin state))
       ((mv mbt$p &) (check-mbt$-call test-term))
       ((when mbt$p) (atc-gen-loop-stmt then-term gin state))
       ((erp (bexpr-gout test))
        (atc-gen-expr-bool test-term
                           (make-bexpr-gin
                            :context gin.context
                            :inscope gin.inscope
                            :prec-tags gin.prec-tags
                            :fn gin.fn
                            :fn-guard gin.fn-guard
                            :compst-var gin.compst-var
                            :thm-index gin.thm-index
                            :names-to-avoid gin.names-to-avoid
                            :proofs gin.proofs)
                           state))
       (formals (formals+ gin.fn wrld))
       ((mv okp affect)
        (b* (((when (member-eq else-term formals)) (mv t (list else-term)))
             ((mv okp terms) (fty-check-list-call else-term))
             ((when (and okp
                         (subsetp-eq terms formals)))
              (mv t terms)))
          (mv nil nil)))
       ((unless okp)
        (reterr
         (msg "The 'else' branch ~x0 of the function ~x1, ~
               which should be the non-recursive branch, ~
               does not have the required form. ~
               See the user documentation."
              else-term gin.fn)))
       ((erp (stmt-gout body))
        (atc-gen-stmt then-term
                      (make-stmt-gin
                       :context gin.context
                       :var-term-alist nil
                       :typed-formals gin.typed-formals
                       :inscope (cons nil gin.inscope)
                       :loop-flag t
                       :affect affect
                       :fn gin.fn
                       :fn-guard gin.fn-guard
                       :compst-var gin.compst-var
                       :prec-fns gin.prec-fns
                       :prec-tags gin.prec-tags
                       :prec-objs gin.prec-objs
                       :thm-index test.thm-index
                       :names-to-avoid test.names-to-avoid
                       :proofs test.proofs)
                      state))
       ((unless (type-case body.type :void))
        (reterr
         (raise "Internal error: ~
                 the loop body ~x0 of ~x1 ~ returns type ~x2."
                then-term gin.fn body.type)))
       (body-stmt (make-stmt-compound :items body.items))
       (stmt (make-stmt-while :test test.expr :body body-stmt))
       ((when (eq gin.measure-for-fn 'quote))
        (reterr
         (raise "Internal error: the measure function is QUOTE.")))
       (measure-call (pseudo-term-fncall gin.measure-for-fn
                                         gin.measure-formals))
       (limit `(binary-+ '1 (binary-+ ,body.limit ,measure-call))))
    (retok (make-lstmt-gout :stmt stmt
                            :test-term test-term
                            :body-term then-term
                            :affect affect
                            :limit-body body.limit
                            :limit-all limit
                            :thm-name nil
                            :thm-index body.thm-index
                            :names-to-avoid body.names-to-avoid
                            :proofs nil)))
  :measure (pseudo-term-count term)
  :guard-hints (("Goal" :in-theory (enable acl2::pseudo-fnsym-p)))
  ///

  (defret stmt-kind-of-atc-gen-loop-stmt
    (equal (stmt-kind (lstmt-gout->stmt gout)) :while)))