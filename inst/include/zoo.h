/*
Header file for using internal C-level facilities
provided by zoo.

This is not 100% designed for end users, so
any user comments and bug reports are very
welcomed.

Copyright Jeffrey A. Ryan 2010
*/

#include <R.h>
#include <Rinternals.h>

#ifndef _Zoo
#define _Zoo

/* internal symbols */
extern SEXP zoo_symbol_index;
extern SEXP zoo_symbol_oclass;
extern SEXP zoo_symbol_frequency;
extern SEXP zoo_symbol_timeDate_format;
extern SEXP zoo_symbol_timeDate_Data;
extern SEXP zoo_symbol_timeDate_FinCenter;

SEXP zoo_lag (SEXP x, SEXP _k, SEXP _pad);
SEXP zoo_coredata (SEXP x, SEXP copyAttr);
#endif
