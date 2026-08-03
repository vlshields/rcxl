#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP C_read_xlsx(SEXP path, SEXP sheet, SEXP col_names, SEXP trim_ws);
SEXP C_sheet_names(SEXP path);

static const R_CallMethodDef call_entries[] = {
    {"C_read_xlsx",   (DL_FUNC) &C_read_xlsx,   4},
    {"C_sheet_names", (DL_FUNC) &C_sheet_names, 1},
    {NULL, NULL, 0}
};

void R_init_rcxl(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, call_entries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
