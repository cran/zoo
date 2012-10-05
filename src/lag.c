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
#include <Rdefines.h>
#include "zoo.h"

SEXP zoo_lag (SEXP x, SEXP _k, SEXP _pad)
{
#ifdef ZOO_DEBUG
Rprintf("zoo_lag\n");
#endif
  SEXP result;
  int i,j;
  double *result_real=NULL;
  int    *result_int=NULL;

  int k=INTEGER(_k)[0] * -1; /* -1 is zoo convention */
  int k_positive = (k > 0) ? 1 : 0;
  int nr = nrows(x);
  int nc = ncols(x);
  int P=0;
  int PAD = INTEGER(coerceVector(_pad,INTSXP))[0];

  if(k > nr)
    error("abs(k) must be less than nrow(x)");

  if(k < 0 && -1*k > nr)
    error("abs(k) must be less than nrow(x)");

  PROTECT(result = allocVector(TYPEOF(x), 
          length(x) - (PAD ? 0 : abs(k)*nc))); P++;
  int nrr = (int)(length(result)/nc);

  if(k_positive) {
  switch(TYPEOF(x)) {
    case REALSXP:
      result_real = REAL(result);
      for(j = 0; j < nc; j++) {
        if(PAD) {
          for(i = 0; i < k; i++)
            result_real[i+(j*nrr)] = NA_REAL;
          memcpy(&REAL(result)[k+(j*nrr)], 
                 &REAL(x)[(j*nrr)], 
                 (nrr-k) * sizeof(double)); 
        } else {
        memcpy(&REAL(result)[(j*nrr)], 
               &REAL(x)[(j*nr)], /* original data need the original 'nr' offset */
               nrr * sizeof(double)); 
        }
      }
      break;
    case INTSXP:
      result_int = INTEGER(result);
      for(j = 0; j < nc; j++) {
        if(PAD) {
          for(i = 0; i < k; i++)
            result_int[i+(j*nrr)] = NA_INTEGER;
          memcpy(&INTEGER(result)[k+(j*nrr)],
                 &INTEGER(x)[(j*nrr)],
                 (nrr-k) * sizeof(int));
        } else {
          memcpy(&INTEGER(result)[(j*nrr)],
                 &INTEGER(x)[(j*nr)],
                 nrr * sizeof(int));
        }
      }
      break;
    case LGLSXP:
      result_int = LOGICAL(result);
      for(j = 0; j < nc; j++) {
        if(PAD) {
          for(i = 0; i < k; i++)
            result_int[i+(j*nrr)] = NA_INTEGER;
          memcpy(&LOGICAL(result)[k+(j*nrr)],
                 &LOGICAL(x)[(j*nrr)],
                 (nrr-k) * sizeof(int));
        } else {
          memcpy(&LOGICAL(result)[(j*nrr)],
                 &LOGICAL(x)[(j*nr)],
                 nrr * sizeof(int));
        }
      }
      break;
    case CPLXSXP:
      for(j = 0; j < nc; j++) {
        if(PAD) {
          for(i = 0; i < k; i++) {
            COMPLEX(result)[i+(j*nrr)].r = NA_REAL;
            COMPLEX(result)[i+(j*nrr)].i = NA_REAL;
          }
          memcpy(&COMPLEX(result)[k+(j*nrr)],
                 &COMPLEX(x)[(j*nrr)],
                 (nrr-k) * sizeof(Rcomplex));
        } else {
          memcpy(&COMPLEX(result)[(j*nrr)],
                 &COMPLEX(x)[(j*nr)],
                 nrr * sizeof(Rcomplex));
        }
      }
      break;
    case RAWSXP:
      for(j = 0; j < nc; j++) {
        if(PAD) {
          for(i = 0; i < k; i++)
            RAW(result)[i+(j*nrr)] = (Rbyte) 0;
          memcpy(&RAW(result)[k+(j*nrr)],
                 &RAW(x)[(j*nrr)],
                 (nrr-k) * sizeof(Rbyte));
        } else {
          memcpy(&RAW(result)[(j*nrr)],
                 &RAW(x)[(j*nr)],
                 nrr * sizeof(Rbyte));
        }
      }
      break;
    case STRSXP:
      for(j = 0; j < nc; j++) {
        if(PAD) {
          for(i = 0; i < k; i++)
            SET_STRING_ELT(result, i+(j*nrr), NA_STRING);
          for(i = 0; i < nrr-k; i++) 
            SET_STRING_ELT(result, k+i+j*nrr, STRING_ELT(x, i+j*nrr));
        } else {
          for(i = 0; i < nrr; i++) 
            SET_STRING_ELT(result, i+j*nrr, STRING_ELT(x, i+j*nr));
        }
      }
      break;
    default:
      error("unsupported type");
      break;
  }
  } else
  if(!k_positive) {
  k = abs(k);
  switch(TYPEOF(x)) {
    case REALSXP:
      result_real = REAL(result);
      for(j =0; j < nc; j++) {
        if(PAD) {
          for(i = nr-k; i < nr; i++)
            result_real[i+(j*nrr)] = NA_REAL;
          memcpy(&REAL(result)[(j*nrr)], 
                 &REAL(x)[k+(j*nrr)], 
                 (nrr-k) * sizeof(double));
        } else {
        memcpy(&REAL(result)[(j*nrr)],
               &REAL(x)[k+(j*nr)],
               nrr * sizeof(double));
        }
      }
      break;
    case INTSXP:
      result_int = INTEGER(result);
      for(j = 0; j < nc; j++) {
        if(PAD) {
          for(i = nr-k; i < nr; i++)
            result_int[i+(j*nrr)] = NA_INTEGER;
          memcpy(&INTEGER(result)[(j*nrr)],
                 &INTEGER(x)[k+(j*nrr)],
                 (nrr-k) * sizeof(int));
        } else {
          memcpy(&INTEGER(result)[(j*nrr)],
                 &INTEGER(x)[k+(j*nr)],
                 nrr * sizeof(int));
        }
      }
      break;
    case LGLSXP:
      result_int = LOGICAL(result);
      for(j = 0; j < nc; j++) {
        if(PAD) {
          for(i = nr-k; i < nr; i++)
            result_int[i+(j*nrr)] = NA_INTEGER;
          memcpy(&LOGICAL(result)[(j*nrr)],
                 &LOGICAL(x)[k+(j*nrr)],
                 (nrr-k) * sizeof(int));
        } else {
          memcpy(&LOGICAL(result)[(j*nrr)],
                 &LOGICAL(x)[k+(j*nr)],
                 nrr * sizeof(int));
        }
      }
      break;
    case CPLXSXP:
      for(j = 0; j < nc; j++) {
        if(PAD) {
          for(i = nr-k; i < nr; i++) {
            COMPLEX(result)[i+(j*nrr)].r = NA_REAL;
            COMPLEX(result)[i+(j*nrr)].i = NA_REAL;
          }
          memcpy(&COMPLEX(result)[(j*nrr)],
                 &COMPLEX(x)[k+(j*nrr)],
                 (nrr-k) * sizeof(Rcomplex));
        } else {
          memcpy(&COMPLEX(result)[(j*nrr)],
                 &COMPLEX(x)[k+(j*nr)],
                 nrr * sizeof(Rcomplex));
        }
      }
      break;
    case RAWSXP:
      for(j = 0; j < nc; j++) {
        if(PAD) {
          for(i = nr-k; i < nr; i++)
            RAW(result)[i+(j*nrr)] = (Rbyte) 0;
          memcpy(&RAW(result)[(j*nrr)],
                 &RAW(x)[k+(j*nrr)],
                 (nrr-k) * sizeof(Rbyte));
        } else {
          memcpy(&RAW(result)[(j*nrr)],
                 &RAW(x)[k+(j*nr)],
                 nrr * sizeof(Rbyte));
        }
      }
      break;
    case STRSXP:
      for(j = 0; j < nc; j++) {
        if(PAD) {
          for(i = nr-k; i < nr; i++)
            SET_STRING_ELT(result, i+(j*nrr), NA_STRING);
          for(i = 0; i < nrr-k; i++)
            SET_STRING_ELT(result, i+(j*nrr), STRING_ELT(x, k+i+(j*nrr)));
        } else {
          for(i = 0; i < nr-k; i++)
            SET_STRING_ELT(result, i+(j*nrr), STRING_ELT(x, k+i+(j*nr)));
        }
      }
      break;
    default:
      error("unsupported type");
      break;
  }
  }

  copyMostAttrib(x,result);
  if(!PAD) {
    // likely unneeded as copyMostAttrib will cover
  //  setAttrib(result, install("index"), getAttrib(x, install("index")));
  //} else {
    SEXP index, newindex;
    PROTECT(index = getAttrib(x, install("index"))); P++;
    if(IS_S4_OBJECT(index)) {
      /* should make this
         1) generic for any S4 object if possible
         2) test for timeDate as this is important
      */
      if(STRING_ELT(getAttrib(index, R_ClassSymbol),0)!=mkChar("timeDate"))
        error("'S4' objects must be of class 'timeDate'");
      index = GET_SLOT(index, install("Data"));
    }
    PROTECT(newindex = allocVector(TYPEOF(index), nrr)); P++;
    switch(TYPEOF(index)) {
      case REALSXP:
        if(k_positive) {
          memcpy(REAL(newindex), &REAL(index)[k], nrr * sizeof(double));
        } else {
          memcpy(REAL(newindex), REAL(index), nrr * sizeof(double));
        }
        break;
      case INTSXP:
        if(k_positive) {
        memcpy(INTEGER(newindex), &INTEGER(index)[k], nrr * sizeof(int));
        } else {
        memcpy(INTEGER(newindex), INTEGER(index), nrr * sizeof(int));
        }
        break;
      default:
        break;
    }
    if(IS_S4_OBJECT(getAttrib(x, install("index")))) {
      /* need to assure that this is timeDate */
      SEXP tmp = PROTECT(getAttrib(x, install("index"))); P++;
      SEXP timeDate = PROTECT(NEW_OBJECT(MAKE_CLASS("timeDate"))); P++;
      copyMostAttrib(index,newindex);
      SET_SLOT(timeDate,install("Data"),newindex);
      SET_SLOT(timeDate,install("format"),
               GET_SLOT(tmp, install("format")));
      SET_SLOT(timeDate,install("FinCenter"),
               GET_SLOT(tmp, install("FinCenter")));
      setAttrib(result, install("index"), timeDate);
    } else {
      copyMostAttrib(index, newindex);
      setAttrib(result, install("index"), newindex);
    }
  } 

  /* reset dims */
  if(!isNull(getAttrib(x, R_DimSymbol))) {
    SEXP dims;
    PROTECT(dims = allocVector(INTSXP, 2)); P++;
    INTEGER(dims)[0] = nrr;
    INTEGER(dims)[1] = nc;
    setAttrib(result, R_DimSymbol, dims); 
    setAttrib(result, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol)); 
  }

  UNPROTECT(P);
  return result;
}

SEXP zoo_lagts (SEXP x, SEXP _k, SEXP _pad) {
  int k = INTEGER(_k)[0]*-1; /* change zoo default negative handling */
  return zoo_lag (x, ScalarInteger(k), _pad);
}
