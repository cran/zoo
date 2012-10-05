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


#include "zoo.h"
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"zoo_lag",               (DL_FUNC) &zoo_lag,                 3},
  {"zoo_coredata",          (DL_FUNC) &zoo_coredata,            2},
  {NULL,                    NULL,                               0}
};

void R_init_zoo(DllInfo *info)
{
  R_registerRoutines(info,
                     NULL,
                     callMethods,
                     NULL,
                     NULL);

  R_useDynamicSymbols(info, TRUE);
#define RegisterZoo(routine) R_RegisterCCallable("zoo",#routine,(DL_FUNC) &routine)

  /* used by external packages linking to internal xts code from C */
  R_RegisterCCallable("zoo","zoo_lag",(DL_FUNC) &zoo_lag);
  R_RegisterCCallable("zoo","zoo_coredata",(DL_FUNC) &zoo_coredata);
}
