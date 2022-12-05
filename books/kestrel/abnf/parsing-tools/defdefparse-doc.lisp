; ABNF (Augmented Backus-Naur Form) Library
;
; Copyright (C) 2022 Kestrel Institute (http://www.kestrel.edu)
;
; License: A 3-clause BSD license. See the LICENSE file distributed with ACL2.
;
; Author: Alessandro Coglio (coglio@kestrel.edu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "ABNF")

(include-book "kestrel/event-macros/xdoc-constructors" :dir :system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defxdoc defdefparse

  :parents (abnf)

  :short "Generator of tools to generate parsing functions."

  :long

  (xdoc::topstring

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (xdoc::evmac-section-intro

    (xdoc::p
     "This macro generates macros, specialized to a given ABNF grammar,
      to generate parsing functions from the rules of the grammar.
      This is not a full parser generator, but it is a start towards one.")

    (xdoc::p
     "Instead of generating grammar-specific macros from this macro,
      an alternative approach is to have generic parsing generation macros
      that take the grammar information as an additional input.
      However, this would make the macros more verbose,
      also because, besides the grammar itself,
      there is some additional information that
      the macros generated by @('defdefparse') are specialized for.
      Thus, we use this macro-generating macro approach,
      where the use of the generated macros is less verbose.")

    (xdoc::p
     "This can be regarded as a meta macro,
      because it is a macro that generates other macro,
      or as a parser generator generator,
      in the sense that it generates tools for parser generation
      (although the tools do not yet form a full parser generator,
      in the commonly used sense of the word).")

    (xdoc::p
     "Currently the implementation of this macro does not perform
      very thorough input validation.
      This will be improved in the future.")

    (xdoc::p
     "Although this tool is fairly preliminary,
      it has been already useful to generate
      a large subset of lexing and parsing functions
      for real (i.e. not toy) programming languages.
      We plan to extend these tools more and more towards
      a customizable parser generator."))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (xdoc::evmac-section-form

    (xdoc::codeblock
     "(defdefparse name"
     "             :package ..."
     "             :grammar ..."
     "             :prefix ..."
     "  )"))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (xdoc::evmac-section-inputs

    (xdoc::desc
     "@('name')"
     (xdoc::p
      "Name of the generated parsing generation tools.")
     (xdoc::p
      "It must be a symbol,
       different from the ones in other @('defdefparse') calls.")
     (xdoc::p
      "This symbol is inserted into the names of the generated macros,
       which have the form @('defparse-name-...').
       Thus, this name is used to differentiate different sets of macros,
       typically generated for different grammars."))

    (xdoc::desc
     "@(':package') &mdash; no default"
     (xdoc::p
      "Package where the generated macros are put.")
     (xdoc::p
      "It must a string that is the name of an existing package.")
     (xdoc::p
      "If @('\"P\"') is the package name,
       the generated macros have names of the form @('p::defparse-name-...')."))

    (xdoc::desc
     "@(':grammar') &mdash; no default"
     (xdoc::p
      "Grammar for which parsing functions are generated
       by the generated @('defparse-name-...') macros.")
     (xdoc::p
      "It must be the name of an existing constant
       that contains a non-empty ABNF grammar
       (i.e. a value of type @(tsee rulelist) that is not @('nil'))."))

    (xdoc::desc
     "@(':prefix') &mdash; no default"
     (xdoc::p
      "Prefix of the parsing functions generated
       by the generated @('p::defparse-name-...') macros.")
     (xdoc::p
      "This is often something like
       @('parse') or @('lex') in some package @('\"Q\"'),
       so that the generated parsing functions have names of the form
       @('q::parse-...') or @('q::lex-...').
       Note that the package @('\"Q\"') may differ from @('\"P\"').")))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (xdoc::evmac-section-generated

    (xdoc::desc
     "@('defparse-name-group-table')"
     (xdoc::p
      "Macro to define a table that maps
       groups in the grammar to the corresponding parsing function names.")
     (xdoc::p
      "It is called as")
     (xdoc::codeblock
      "(defparse-name-group-table"
      "  \"<group1>\" symbol1"
      "  ..."
      "  \"<groupN>\" symbolN"
      "  )")
     (xdoc::p
      "where each @('<groupI>') is a group written in ABNF concrete syntax
       and each @('symbolI') is the name of the function
       without the prefix specified in the @(':prefix') input.
       That is, the name of the parsing function
       for the group described by @('<groupI>') is @('prefix-symbolI').")
     (xdoc::p
      "The @('<group1>'), ..., @('<groupN>') strings
       are parsed via the verified @(tsee parse-group),
       obtaining @(tsee alternation)s that must be all distinct.")
     (xdoc::p
      "The parsing functions @('prefix-symbol1'), ..., @('prefix-symbolN')
       do not exist yet when @('defparse-name-group-table') is called.
       The call merely defines the names of these functions,
       which are created later via @('defparse-name-group') (see below)."))

    (xdoc::desc
     "@('defparse-name-option-table')"
     (xdoc::p
      "Macro to define a table that maps
       options in the grammar to the corresponding parsing function names.")
     (xdoc::p
      "It is called as")
     (xdoc::codeblock
      "(defparse-name-option-table"
      "  \"<option1>\" symbol1"
      "  ..."
      "  \"<optionN>\" symbolN"
      "  )")
     (xdoc::p
      "where each @('<optionI>') is an option written in ABNF concrete syntax
       and each @('symbolI') is the name of the function
       without the prefix specified in the @(':prefix') input.
       That is, the name of the parsing function
       for the option described by @('<optionI>') is @('prefix-symbolI').")
     (xdoc::p
      "The @('<option1>'), ..., @('<optionN>') strings
       are parsed via the verified @(tsee parse-option),
       obtaining @(tsee alternation)s that must be all distinct.")
     (xdoc::p
      "The parsing functions @('prefix-symbol1'), ..., @('prefix-symbolN')
       do not exist yet when @('defparse-name-option-table') is called.
       The call merely defines the names of these functions,
       which are created later via @('defparse-name-option') (see below)."))

    (xdoc::desc
     "@('defparse-name-repetition-table')"
     (xdoc::p
      "Macro to define a table that maps
       repetitions in the grammar to the corresponding parsing function names.")
     (xdoc::p
      "It is called as")
     (xdoc::codeblock
      "(defparse-name-repetition-table"
      "  \"<repetition>\" symbol1"
      "  ..."
      "  \"<repetitionN>\" symbolN"
      "  )")
     (xdoc::p
      "where each @('<repetitionI>') is a group written in ABNF concrete syntax
       and each @('symbolI') is the name of the function
       without the prefix specified in the @(':prefix') input.
       That is, the name of the parsing function
       for the repetition described by @('<repetition>')
       is @('prefix-symbolI').")
     (xdoc::p
      "The @('<repetition1>'), ..., @('<repetitionN>') strings
       are parsed via the verified @(tsee parse-repetition),
       obtaining @(tsee repetition)s that must be all distinct.")
     (xdoc::p
      "The parsing functions @('prefix-symbol1'), ..., @('prefix-symbolN')
       do not exist yet when @('defparse-name-repetition-table') is called.
       The call merely defines the names of these functions,
       which are created later via
       @('defparse-name-*-rulename') and @('defparse-name-*-group')
       (see below)."))

    (xdoc::desc
     "@('defparse-name-rulename')"
     (xdoc::p
      "Macro to generate a parsing function for a rule name in the grammar.")
     (xdoc::p
      "It is called as")
     (xdoc::codeblock
      "(defparse-name-rulename \"<rulename>\") :order ...")
     (xdoc::p
      "where @('<rulename>') is a rule name in the grammar
       and @(':order'), which is optional,
       specifies a permutation of the list @('(1 ... N)'),
       where @('N') is the number of alternatives
       that defines the rule name in the grammar.")
     (xdoc::p
      "The parsing functions for all the
       rule names, groups, options, and repetitions
       that occur in the alternatives that define the rule name
       must have already been generated when this call is made.")
     (xdoc::p
      "If present, @(':order') specifies the order in which
       the alternatives that define the rule name
       is attempted by the generated parsing function for the rule name.")
     (xdoc::p
      "The name of the generated parsing function is @('prefix-rulename').")
     (xdoc::p
      "The generated parsing function looks up the rule name in the grammar,
       obtaining the alternation that defines the rule name
       (this includes any incremental rules; see @(tsee lookup-rulename)).
       The generated function attempts to parse each alternative in order
       (in the order in which they appear in the grammar,
       unless @(':order') is used, see below),
       backtracking if the parsing of an alternative fails,
       stopping when either the parsing of an alternative succeeds
       or there are no more alternatives (in which case parsing fails).
       The @(':order') input, if present,
       specifies a reordering of the alternatives:
       the numbers @('1') to @('N') denote the alternatives
       as they appear in the grammar;
       the supplied permutation provides the reordering.
       The reordering is useful to enforce extra-grammatical requirements
       that provide disambiguation (e.g. parse the longest parsable text),
       or for greater efficiency (e.g. parse more common alternatives first).")
     (xdoc::p
      "The generated parsing function
       attempts to parse each alternative,
       which is a concatenation of repetitions,
       by attempting to parse the repetitions in order.
       Repetitions are often singletons, i.e. they are effectively elements:
       in this case, the function directly attempts to parse the element.
       An element may be a rule name,
       in which case the corresponding parsing function is called,
       whose name is known because it is derived from the rule name,
       similarly to the name of the current parsing function.
       For elements that are groups or options,
       or for repetitions that are not singletons,
       the name of the corresponding parsing function
       must be present in the tables introduced via the macros above."))

    (xdoc::desc
     "@('defparse-name-*-rulename')"
     (xdoc::p
      "Macro to generate a parsing function for
       a repetition of zero or more instances of a rule name.")
     (xdoc::p
      "It is called as")
     (xdoc::codeblock
      "(defparse-name-*-rulename \"<rulename>\")")
     (xdoc::p
      "where @('<rulename>') is a rule name in the grammar,
       for which a parsing function must have already been generated
       when this call is made.")
     (xdoc::p
      "The repetition must be
       in the table generated via @('defparse-name-repetition-table')
       (see above).
       The name of the generated parsing function
       is the corresponding one in the table."))

    (xdoc::desc
     "@('defparse-name-group')"
     (xdoc::p
      "Macro to generate a parsing function for a group in the grammar.")
     (xdoc::p
      "It is called as")
     (xdoc::codeblock
      "(defparse-name-group \"<group>\") :order ...")
     (xdoc::p
      "where @('<group>') is a group in the grammar
       written in ABNF concrete syntax,
       and @(':order'), which is optional,
       specifies a permutation of the list @('(1 ... N)'),
       where @('N') is the number of alternatives
       that forms the group.")
     (xdoc::p
      "The parsing functions for all the
       rule names, groups, options, and repetitions
       that occur in the group
       must have already been generated when this call is made.")
     (xdoc::p
      "If present, @(':order') specifies the order in which
       the alternatives that form the group
       is attempted by the generated parsing function for the group.")
     (xdoc::p
      "The group must be
       in the table generated via @('defparse-name-group-table')
       (see above).
       The name of the generated parsing function
       is the corresponding one in the table.")
     (xdoc::p
      "The generated parsing function works similarly to
       the parsing functions for rule names described above.
       The difference is that the alternation is the one that forms the group,
       instead of being retrieved from the grammar.
       The optional @(':order') input serves to change the order in which
       the alternatives are attempted to parse."))

    (xdoc::desc
     "@('defparse-name-*-group')"
     (xdoc::p
      "Macro to generate a parsing function for
       a repetition of zero or more instances of a group.")
     (xdoc::p
      "It is called as")
     (xdoc::codeblock
      "(defparse-name-*-group \"<group>\")")
     (xdoc::p
      "where @('<group>') is a group in the grammar
       written in ABNF concrete syntax,
       for which a parsing function must have already been generated
       when this call is made.")
     (xdoc::p
      "The repetition must be
       in the table generated via @('defparse-name-repetition-table')
       (see above).
       The name of the generated parsing function
       is the corresponding one in the table."))

    (xdoc::desc
     "@('defparse-name-option')"
     (xdoc::p
      "Macro to generate a parsing function for an option in the grammar.")
     (xdoc::p
      "It is called as")
     (xdoc::codeblock
      "(defparse-name-option \"<option>\") :order ...")
     (xdoc::p
      "where @('<option>') is an option in the grammar
       written in ABNF concrete syntax,
       and @(':order'), which is optional,
       specifies a permutation of the list @('(1 ... N)'),
       where @('N') is the number of alternatives
       that forms the option")
     (xdoc::p
      "The parsing functions for all the
       rule names, groups, options, and repetitions
       that occur in the option
       must have already been generated when this call is made.")
     (xdoc::p
      "If present, @(':order') specifies the order in which
       the alternatives that form the option
       is attempted by the generated parsing function for the option")
     (xdoc::p
      "The group must be
       in the table generated via @('defparse-name-option-table')
       (see above).
       The name of the generated parsing function
       is the corresponding one in the table.")
     (xdoc::p
      "The generated parsing function works similarly to
       the ones generated for groups.
       The key difference is that, if no alternative succeeds,
       the parsing function for an option succeeds,
       because it just means that the option was absent,
       while instead the parsing function for a group fails in that case."))

    (xdoc::p
     "Singleton repetitions do not need to be in the table for repetitions.
      Singleton repetitions are handled as their underlying elements
      in the generated parsing functions.")

    (xdoc::p
     "The parsing functions must be generated in the usual ACL2 order,
      i.e. if a parsing function must call another one,
      the latter must be generated before generating the former.
      Thus, these macros currently cannot generate
      mutually recursive parsing functions.
      This is clearly a severe limitation for typical parsing,
      but it is still useful for generating lexers,
      which often have less mutual recursion.
      Single recursion is supported, in the form or repetitions.
      We plan to extend these parser generation tools
      to support mutual recursion.")

    (xdoc::p
     "Not all the parsing functions need to be generated via the macros.
      Some may be handwritten, e.g. if they are mutually recursive.
      So long as the names of these handwritten functions
      have the form @('prefix-rulename') for a rule name @('rulename')
      or are recorded in the tables for groups, options, and repetitions,
      generated parsing functions can seamlessly call handwritten ones.")

    (xdoc::p
     "Numeric and character terminal notations
      are handled automatically by these parser generation tools.
      That is, code to parse instances of those is automatically generated;
      the user does not have to generate any parsing functions for those,
      and does not need to specify any parsing function names for those
      (no parsing functions are generated for those;
      the parsing code for those is generated
      within larger parsing functions).")

    (xdoc::p
     "All three tables, for groups and options and repetitions,
      must be defined, even if any of them is empty,
      i.e. if the user does not want or need to generate or use
      parsing functions for groups or options or repetitions.")

    (xdoc::p
     "The recursive functions generated by these parsing generation tools
      are proved to terminate, via the length of the input as measure.
      To support these termination proofs,
      the tools generate linear rules for each function:
      one says that the length of the remaining input
      is always less than or equal to the length of the input
      (whether the function succeeds or fails);
      the other one says that the length of the remaining input
      is strictly less than the length of the input
      when the function succeeds (i.e. when it does not return an error).
      Actually, this second rule is generated only for functions
      that are expected to consume at least some input when they succeed:
      this is not the case for
      function that parse options
      (because the option may be absent),
      function that parse zero or more repetitions
      (because there may be zero repetition),
      and for functions that parse nullable rule names,
      i.e. rule names whose definition may be satisfied
      by the empty sequence of natural numbers.
      For now the tools use a very conservative notion of nullability:
      they only regard as nullable a rule name whose definition
      is a repetition of zero or more of something.
      In any case, if a parsing function is hand-written,
      it must include (one or two) linear rules as appropriate,
      so that generated functions that call the hand-written ones
      can also generate similar rules,
      and can be proved to terminate if recursive.")

    (xdoc::p
     "The functions generated by these parser generation tools
      are defined via @(tsee define) and include @(':hooks (:fix)').
      The latter causes the generation of fixing theorems,
      which say that the functions fix their arguments to their types.
      These theorems are expected to be proved automatically
      so long as all the parsing functions called by the generated functions
      include those fixing theorems.
      Thus, if some parsing functions are hand-written,
      and called by generated parsing functions,
      the hand-written functions must include @(':hooks (:fix)')
      in order for all the fixing theorems to be proved."))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (xdoc::evmac-section
    xdoc::*evmac-section-redundancy-title*

    (xdoc::p
     "A call of @('defdefparse') is redundant if and only if
      it is identical to a previous successful call of @('defdefparse')
      with the exact same arguments."))))