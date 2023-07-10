#include "functions.h"
#include "parser.h"
#include "statements.h"
#include "types.h"

// 6/27/18
// I just eliminated two features that are no longer used: (1) CtoS mode, in
// which a program was printed in a form acceptable to CtoS, and (2) SystemC
// registers, which have been replaced by those of Algorithmic C, hence the
// change in the name of the entire system from MASC (Modeling Algorithms in
// SystemC) to RAC (Restricted Algorithmic C).  This simplified the parser
// considerably, and I suspect I've left a bunch of dead or useless code lying
// around, to be dealt with later.

//***********************************************************************************
// S-expressions
//***********************************************************************************

void
Plist::display (ostream &os) const
{
  os << '(';

  bool first = true;
  for_each (list, [&] (auto v) {
    if (!first)
      {
        os << ' ';
      }
    v->display (os);
    first = false;
  });

  os << ')';
}

// Symbol constants used in translation:

Symbol s_ag ("ag");
Symbol s_as ("as");
Symbol s_ash ("ash");
Symbol s_assert ("assert");
Symbol s_assign ("assign");
Symbol s_bitn ("bitn");
Symbol s_bits ("bits");
Symbol s_block ("block");
Symbol s_break ("break");
Symbol s_case ("case");
Symbol s_declare ("declare");
Symbol s_default ("default");
Symbol s_divide ("/");
Symbol s_expt ("expt");
Symbol s_fl ("fl");
Symbol s_false ("false$");
Symbol s_true ("true$");
Symbol s_if ("if");
Symbol s_if1 ("if1");
Symbol s_for ("for");
Symbol s_list ("list");
Symbol s_logand ("logand");
Symbol s_logand1 ("logand1");
Symbol s_logeq ("log=");
Symbol s_loggeq ("log>=");
Symbol s_loggreater ("log>");
Symbol s_logior ("logior");
Symbol s_logior1 ("logior1");
Symbol s_logleq ("log<=");
Symbol s_logless ("log<");
Symbol s_logneq ("log<>");
Symbol s_lognot ("lognot");
Symbol s_lognot1 ("lognot1");
Symbol s_logxor ("logxor");
Symbol s_minus ("-");
Symbol s_mv ("mv");
Symbol s_mv_assign ("mv-assign");
Symbol s_nth ("nth");
Symbol s_nil ("nil");
Symbol s_null ("null");
Symbol s_plus ("+");
Symbol s_quote ("quote");
Symbol s_rem ("rem");
Symbol s_return ("return");
Symbol s_t ("t");
Symbol s_times ("*");
Symbol s_floor ("floor");
Symbol s_slash ("/");
Symbol s_setbitn ("setbitn");
Symbol s_setbits ("setbits");
Symbol s_si ("si");
Symbol s_switch ("switch");
// Symbol s_temp[4] = {Symbol("temp-0"), Symbol("temp-1"), Symbol("temp-2"),
// Symbol("temp-3")};

// class SymDec (symbol declaration)
// ------------

// Derived classes: EnumConstDec, VarDec, and TempParamDec.

// Data members: Symbol* sym; Type *type; Expression *init; (init is optional)

SymDec::SymDec (const char *n, Type *t, Expression *i)
    : sym (new Symbol (n)), type (t), init (i)
{
}

void
SymDec::displaySymDec (ostream &os) const
{
  type->displayVarType (os);
  os << " ";
  type->displayVarName (getname (), os);
  if (init)
    {
      os << " = ";
      init->display (os);
    }
}

bool
SymDec::isConst ()
{
  return false;
} // overridden by EnumConstDec and ConstDec

int
SymDec::evalConst ()
{
  assert (init);
  return init->evalConst ();
}

bool
SymDec::isGlobal ()
{
  return false;
}

bool
SymDec::isROM ()
{
  return false;
}

Sexpression *
SymDec::ACL2SymExpr ()
{ // Sexpression for a reference to this symbol.
  assert (!"Undefined method: ACL2SymExpr");
  return nullptr;
}

//***********************************************************************************
// Program
//***********************************************************************************

Program::Program ()
    : typeDefs (List<DefinedType>::empty ()),
      constDecs (List<ConstDec>::empty ()),
      templates (List<Template>::empty ()), funDefs (List<FunDef>::empty ())
{
}

void
Program::displayTypeDefs (ostream &os, DispMode mode) const
{
  // Note that type definitions are used in generating S-expressions for
  // constant declarations and function definitions, but are not represented
  // explicitly in the ACL2 translation.
  if (mode == DispMode::rac)
    {
      for_each (typeDefs, [&os] (auto v) { v->displayDef (os); });
    }
}

// TODO constify ACL2Exp, then this can become const
void
Program::displayConstDecs (ostream &os, DispMode mode)
{
  if (mode == DispMode::rac)
    {
      for_each (constDecs, [&os] (auto v) { v->display (os); });
    }
  else
    {
      for_each (constDecs, [&os] (auto v) { v->ACL2Expr ()->display (os); });
    }
}

// Why this one is not defined
//  void displayTemplates(ostream& os, DispMode mode, const char *prefix="");

void
Program::displayFunDefs (ostream &os, DispMode mode)
{
  if (mode == DispMode::rac)
    {
      for_each (funDefs, [&os] (auto v) { v->display (os); });
    }
  else
    {
      for_each (funDefs, [&os] (auto v) { v->displayACL2Expr (os); });
    }
}

void
Program::displayFunDecs (ostream &os) const
{
  for_each (funDefs, [&os] (auto v) { v->displayDec (os); });
}

void
Program::display (ostream &os, DispMode mode)
{
  displayTypeDefs (os, mode);
  os << "\n";
  displayConstDecs (os, mode);
  os << "\n";
  displayFunDefs (os, mode);
  os << "\n";
}

bool
Program::isEmpty () const
{
  return !funDefs->length ();
}