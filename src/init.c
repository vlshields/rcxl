#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP C_read_xlsx(SEXP path, SEXP sheet, SEXP col_names, SEXP trim_ws,
                 SEXP win, SEXP col_types, SEXP na);
SEXP C_read_xlsx_all(SEXP path, SEXP sheets, SEXP col_names, SEXP trim_ws,
                     SEXP win, SEXP col_types, SEXP na);
SEXP C_sheet_names(SEXP path);
void rcxl_crc32_init(void);

static const R_CallMethodDef call_entries[] = {
    {"C_read_xlsx",     (DL_FUNC) &C_read_xlsx,     7},
    {"C_read_xlsx_all", (DL_FUNC) &C_read_xlsx_all, 7},
    {"C_sheet_names",   (DL_FUNC) &C_sheet_names,   1},
    {NULL, NULL, 0}
};

void R_init_rcxl(DllInfo *dll)
{
    rcxl_crc32_init();
    R_registerRoutines(dll, NULL, call_entries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
