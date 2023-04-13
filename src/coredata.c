/*
#   zoo:
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
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


#include <R.h>
#include <Rinternals.h>

SEXP zoo_coredata (SEXP x, SEXP copyAttr)
{
  /* copyAttr is a LGLSXP flag to indicate whether all
     attributes are to be left intact.  This provides
     compatability with xts, by stripping all attributes
     if desired, without the overhead or adding then
     removing
  */
  SEXP result;
  R_xlen_t i, j, ncs, nrs;
  int P=0;
  PROTECT(result = allocVector(TYPEOF(x), xlength(x))); P++;
  switch( TYPEOF(x)) {
    case REALSXP:
      memcpy(REAL(result), REAL(x), xlength(result) * sizeof(double));
      break;
    case INTSXP:
      memcpy(INTEGER(result), INTEGER(x), xlength(result) * sizeof(int));
      break;
    case LGLSXP:
      memcpy(LOGICAL(result), LOGICAL(x), xlength(result) * sizeof(int));
      break;
    case CPLXSXP:
      memcpy(COMPLEX(result), COMPLEX(x), xlength(result) * sizeof(Rcomplex));
      break;
    case STRSXP:
      ncs = ncols(x); nrs = nrows(x);
      for(j=0; j< ncs; j++)
      for(i=0; i< nrs; i++)
        SET_STRING_ELT(result, i+j*nrs, STRING_ELT(x, i+j*nrs));
      break;
    case RAWSXP:
      memcpy(RAW(result), RAW(x), xlength(result) * sizeof(unsigned char));
      break;
    default:
      error("currently unsupported data type");
      break;
  }
  if( !isNull(getAttrib(x, R_DimSymbol))) {
    setAttrib(result, R_DimSymbol, getAttrib(x, R_DimSymbol));
    if( !isNull(getAttrib(x, R_DimNamesSymbol)) ) {  
      setAttrib(result, R_DimNamesSymbol, getAttrib(x,R_DimNamesSymbol));
    }
  } else {
    setAttrib(result, R_NamesSymbol, getAttrib(x, R_NamesSymbol));
  }
  if( asLogical(copyAttr)) {
    copyMostAttrib(x,result);
    SEXP oclass = PROTECT(getAttrib(x, install("oclass"))); P++;
    setAttrib(result, install("class"), oclass);
  }
  setAttrib(result, install("index"),     R_NilValue);
  setAttrib(result, install("oclass"),    R_NilValue);
  setAttrib(result, install("frequency"), R_NilValue);

  UNPROTECT(P);
  return result;
}
