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
#include <zoo.h>

SEXP zoo_lag (SEXP x, SEXP _k, SEXP _pad)
{
#ifdef ZOO_DEBUG
Rprintf("zoo_lag\n");
#endif
  SEXP result;
  R_xlen_t i,j;

  int k = asInteger(_k) * -1; /* -1 is zoo convention */
  R_xlen_t nr = nrows(x);
  R_xlen_t nc = ncols(x);
  int P=0;
  int PAD = asInteger(_pad);

  R_xlen_t k_abs = abs(k);                     /* magnitude of the lag */
  R_xlen_t k_src = (k > 0) ? 0 : k_abs;        /* offset into x, "nrr stride" units */
  R_xlen_t k_dst = (k > 0 && PAD) ? k_abs : 0; /* offset into result */
  R_xlen_t k_pad = (k > 0) ? 0 : (nr - k_abs); /* where NA/pad values start */

  if(k_abs > nr)
    error("abs(k) must be less than nrow(x)");

  PROTECT(result = allocVector(TYPEOF(x), 
          xlength(x) - (PAD ? 0 : abs(k)*nc))); P++;

  R_xlen_t nrr;
  if(xlength(result) > 0)
    nrr = (R_xlen_t)(xlength(result)/nc);
  else  /* handle zero-length objects */
    nrr = nr - (PAD ? 0 : k_abs);
  R_xlen_t n_copy = nr - k_abs;

  switch (TYPEOF(x)) {
      case REALSXP:
          for (j = 0; j < nc; j++) {
              if (PAD) {
                  for (i = 0; i < k_abs; i++) {
                      REAL(result)[k_pad+i+(j*nrr)] = NA_REAL;
                  }
              }
              memcpy(&REAL(result)[k_dst+(j*nrr)], &REAL(x)[k_src+(j*nr)], sizeof(double) * n_copy);
          }
          break;
      case INTSXP:
          for (j = 0; j < nc; j++) {
              if (PAD) {
                  for (i = 0; i < k_abs; i++) {
                      INTEGER(result)[k_pad+i+(j*nrr)] = NA_INTEGER;
                  }
              }
              memcpy(&INTEGER(result)[k_dst+(j*nrr)], &INTEGER(x)[k_src+(j*nr)], sizeof(int) * n_copy);
          }
          break;
      case LGLSXP:
          for (j = 0; j < nc; j++) {
              if (PAD) {
                  for (i = 0; i < k_abs; i++) {
                      LOGICAL(result)[k_pad+i+(j*nrr)] = NA_LOGICAL;
                  }
              }
              memcpy(&LOGICAL(result)[k_dst+(j*nrr)], &LOGICAL(x)[k_src+(j*nr)], sizeof(int) * n_copy);
          }
          break;
      case CPLXSXP:
          for (j = 0; j < nc; j++) {
              if (PAD) {
                  for (i = 0; i < k_abs; i++) {
                      COMPLEX(result)[k_pad+i+(j*nrr)].r = NA_REAL;
                      COMPLEX(result)[k_pad+i+(j*nrr)].i = NA_REAL;
                  }
              }
              memcpy(&COMPLEX(result)[k_dst+(j*nrr)], &COMPLEX(x)[k_src+(j*nr)], sizeof(Rcomplex) * n_copy);
          }
          break;
      case RAWSXP:
          for(j = 0; j < nc; j++) {
              if (PAD) {
                  for (i = 0; i < k_abs; i++) {
                      RAW(result)[k_pad+i+(j*nrr)] = (Rbyte)0;
                  }
              }
              memcpy(&RAW(result)[k_dst+(j*nrr)], &RAW(x)[k_src+(j*nr)], sizeof(Rbyte) * n_copy);
          }
          break;
      case STRSXP:
          for(j = 0; j < nc; j++) {
              if (PAD) {
                  for (i = 0; i < k_abs; i++) {
                      SET_STRING_ELT(result, k_pad+i+(j*nrr), NA_STRING);
                  }
              }
              for(i = 0; i < n_copy; i++) {
                  SET_STRING_ELT(result, k_dst+i+(j*nrr), STRING_ELT(x, k_src+i+(j*nr)));
              }
          }
          break;
      default:
          error("unsupported type");
          break;
  }

  copyMostAttrib(x, result);  /* copy all attr *exept* names, dim, dimnames */
  if(!PAD) {
    /* need to shorten the index because the result was not padded with NA */
    SEXP idx_x    = PROTECT(getAttrib(x, zoo_symbol_index)); P++;
    SEXP idx_data = idx_x;
    if (isS4(idx_x)) {
      /* should make this
         1) generic for any S4 object if possible
         2) test for timeDate as this is important
      */
      SEXP idx_class = PROTECT(getAttrib(idx_x, R_ClassSymbol)); P++;
      if (STRING_ELT(idx_class, 0) != mkChar("timeDate"))
        error("'S4' objects must be of class 'timeDate'");
      idx_data = PROTECT(GET_SLOT(idx_x, zoo_symbol_timeDate_Data)); P++;
    }
    int idx_type = TYPEOF(idx_data);
    SEXP newindex = PROTECT(allocVector(idx_type, nrr)); P++;
    R_xlen_t k_idx = (k > 0) ? k_abs : 0;
    switch (idx_type) {
      case REALSXP:
        memcpy(REAL(newindex), &REAL(idx_data)[k_idx], nrr * sizeof(double));
        break;
      case INTSXP:
        memcpy(INTEGER(newindex), &INTEGER(idx_data)[k_idx], nrr * sizeof(int));
        break;
      default:
          error("unsupported index type");
        break;
    }
    if (isS4(idx_x)) {
      SEXP class = PROTECT(MAKE_CLASS("timeDate")); P++;
      SEXP timeDate = PROTECT(NEW_OBJECT(class)); P++;
      copyMostAttrib(idx_data, newindex);
      SET_SLOT(timeDate, zoo_symbol_timeDate_Data,newindex);
      SEXP format = PROTECT(GET_SLOT(idx_x, zoo_symbol_timeDate_format)); P++;
      SET_SLOT(timeDate, zoo_symbol_timeDate_format, format);
      SEXP finCenter = PROTECT(GET_SLOT(idx_x, zoo_symbol_timeDate_FinCenter)); P++;
      SET_SLOT(timeDate, zoo_symbol_timeDate_FinCenter, finCenter);
      setAttrib(result, zoo_symbol_index, timeDate);
    } else {
      copyMostAttrib(idx_data, newindex);
      setAttrib(result, zoo_symbol_index, newindex);
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
  int k_pos = asInteger(_k)*-1; /* change zoo default negative handling */
  SEXP k = PROTECT(ScalarInteger(k_pos));
  SEXP ans = zoo_lag (x, k, _pad);
  UNPROTECT(1);
  return ans;
}
