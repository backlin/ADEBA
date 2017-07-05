#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

// Referring to the generated functions in RcppExports.cpp
extern SEXP adeba_find_constants(SEXP);
extern SEXP adeba_is_constant(SEXP);
extern SEXP adeba_get_bandwidths(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"adeba_find_constants", (DL_FUNC) &adeba_find_constants, 1},
    {"adeba_is_constant", (DL_FUNC) &adeba_is_constant, 1},
    {"adeba_get_bandwidths", (DL_FUNC) &adeba_get_bandwidths, 3},
    {NULL, NULL, 0}
};

void R_init_adeba(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

