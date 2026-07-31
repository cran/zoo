/*
#   xts: eXtensible time-series 
#
#   Copyright (C) 2010  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


#include <zoo.h>
#include <R_ext/Rdynload.h>

/* define 'symbol' variables that are declared extern in zoo.h */
SEXP zoo_symbol_index;
SEXP zoo_symbol_oclass;
SEXP zoo_symbol_frequency;
SEXP zoo_symbol_timeDate_format;
SEXP zoo_symbol_timeDate_Data;
SEXP zoo_symbol_timeDate_FinCenter;

static const
R_CallMethodDef callMethods[] = {
  {"zoo_lag",               (DL_FUNC) &zoo_lag,                 3},
  {"zoo_coredata",          (DL_FUNC) &zoo_coredata,            2},
  {NULL,                    NULL,                               0}
};

/*
 * Taken from R/src/main/names.c
 *   "Set up a set of globals so that a symbol table search can be
 *    avoided when matching something like dim or dimnames."
 *
 * This also prevents flags from rchk's maacheck (Multiple-Allocating-
 * Arguments) tool for calls like:
 *   setAttrib(result, xts_IndexSymbol, getAttrib(x, xts_IndexSymbol));
 */
static void SymbolShortcuts(void)
{
  zoo_symbol_index = install("index");
  zoo_symbol_oclass = install("oclass");
  zoo_symbol_frequency = install("frequency");
  zoo_symbol_timeDate_format = install("format");
  zoo_symbol_timeDate_Data = install("Data");
  zoo_symbol_timeDate_FinCenter = install("FinCenter");
}

void R_init_zoo(DllInfo *info)
{
  SymbolShortcuts();
  R_registerRoutines(info,
                     NULL,
                     callMethods,
                     NULL,
                     NULL);

  R_useDynamicSymbols(info, FALSE);
#define RegisterZoo(routine) R_RegisterCCallable("zoo",#routine,(DL_FUNC) &routine)

  /* used by external packages linking to internal xts code from C */
  R_RegisterCCallable("zoo","zoo_lag",(DL_FUNC) &zoo_lag);
  R_RegisterCCallable("zoo","zoo_coredata",(DL_FUNC) &zoo_coredata);
}
