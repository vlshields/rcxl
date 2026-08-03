#include <string.h>
#include <stdlib.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>
#include "miniz.h"


typedef struct { char *p; size_t n; } buf_t;
typedef struct { const char *p; int n; } str_t;
typedef struct { const char *n; int nlen; const char *v; int vlen; } attr_t;

static const char *sfind(const char *p, const char *end, const char *pat)
{
    size_t n = strlen(pat);
    if (p == NULL || (size_t)(end - p) < n) return NULL;
    const char *last = end - n;
    for (;;) {
        p = memchr(p, pat[0], (size_t)(last - p) + 1);
        if (!p) return NULL;
        if (memcmp(p, pat, n) == 0) return p;
        if (++p > last) return NULL;
    }
}

/* find "<tag" where the next char ends the tag name */
static const char *find_tag(const char *p, const char *end, const char *tag)
{
    size_t n = strlen(tag);
    for (;;) {
        p = sfind(p, end, tag);
        if (!p) return NULL;
        const char *b = p + n;
        if (b >= end || *b == ' ' || *b == '>' || *b == '/' ||
            *b == '\t' || *b == '\r' || *b == '\n')
            return p;
        p++;
    }
}

static const char *next_attr(const char *p, const char *end, attr_t *a)
{
    while (p < end && (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n')) p++;
    if (p >= end || *p == '>' || *p == '/') return NULL;
    a->n = p;
    while (p < end && *p != '=' && *p != ' ' && *p != '>' && *p != '/') p++;
    a->nlen = (int)(p - a->n);
    while (p < end && *p != '=') {
        if (*p == '>') return NULL;
        p++;
    }
    if (p >= end) return NULL;
    p++;
    while (p < end && (*p == ' ' || *p == '\t')) p++;
    if (p >= end || (*p != '"' && *p != '\'')) return NULL;
    char q = *p++;
    a->v = p;
    while (p < end && *p != q) p++;
    if (p >= end) return NULL;
    a->vlen = (int)(p - a->v);
    return p + 1;
}

static int attr_is(const attr_t *a, const char *name)
{
    size_t n = strlen(name);
    return (size_t)a->nlen == n && memcmp(a->n, name, n) == 0;
}

static const char *tag_close(const char *p, const char *end, int *self)
{
    *self = 0;
    while (p < end && *p != '>') {
        if (*p == '/') *self = 1;
        p++;
    }
    return p < end ? p + 1 : end;
}

static long slice_long(const char *s, int len)
{
    long v = 0;
    int i = 0;
    for (; i < len && s[i] >= '0' && s[i] <= '9'; i++) v = v * 10 + (s[i] - '0');
    return v;
}

/* column part of a cell ref like "BC12" -> 0-based index */
static int ref_col(const char *s, int len)
{
    int c = 0, i;
    for (i = 0; i < len; i++) {
        char ch = s[i];
        if (ch >= 'A' && ch <= 'Z') c = c * 26 + (ch - 'A' + 1);
        else if (ch >= 'a' && ch <= 'z') c = c * 26 + (ch - 'a' + 1);
        else break;
    }
    return c - 1;
}

static int utf8_put(unsigned int cp, char *out)
{
    if (cp < 0x80) { out[0] = (char)cp; return 1; }
    if (cp < 0x800) {
        out[0] = (char)(0xC0 | (cp >> 6));
        out[1] = (char)(0x80 | (cp & 0x3F));
        return 2;
    }
    if (cp < 0x10000) {
        out[0] = (char)(0xE0 | (cp >> 12));
        out[1] = (char)(0x80 | ((cp >> 6) & 0x3F));
        out[2] = (char)(0x80 | (cp & 0x3F));
        return 3;
    }
    out[0] = (char)(0xF0 | (cp >> 18));
    out[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
    out[2] = (char)(0x80 | ((cp >> 6) & 0x3F));
    out[3] = (char)(0x80 | (cp & 0x3F));
    return 4;
}

static int unescape_into(const char *s, int len, char *dst)
{
    const char *end = s + len;
    char *d = dst;
    while (s < end) {
        if (*s != '&') { *d++ = *s++; continue; }
        int rem = (int)(end - s);
        if (rem >= 5 && memcmp(s, "&amp;", 5) == 0) { *d++ = '&'; s += 5; }
        else if (rem >= 4 && memcmp(s, "&lt;", 4) == 0) { *d++ = '<'; s += 4; }
        else if (rem >= 4 && memcmp(s, "&gt;", 4) == 0) { *d++ = '>'; s += 4; }
        else if (rem >= 6 && memcmp(s, "&quot;", 6) == 0) { *d++ = '"'; s += 6; }
        else if (rem >= 6 && memcmp(s, "&apos;", 6) == 0) { *d++ = '\''; s += 6; }
        else if (rem >= 4 && s[1] == '#') {
            const char *q = s + 2;
            unsigned int cp = 0;
            if (*q == 'x' || *q == 'X') {
                q++;
                while (q < end && *q != ';') {
                    char c = *q++;
                    cp <<= 4;
                    if (c >= '0' && c <= '9') cp |= (unsigned)(c - '0');
                    else if (c >= 'a' && c <= 'f') cp |= (unsigned)(c - 'a' + 10);
                    else if (c >= 'A' && c <= 'F') cp |= (unsigned)(c - 'A' + 10);
                }
            } else {
                while (q < end && *q != ';') {
                    if (*q >= '0' && *q <= '9') cp = cp * 10 + (unsigned)(*q - '0');
                    q++;
                }
            }
            if (q < end) q++;
            d += utf8_put(cp, d);
            s = q;
        }
        else *d++ = *s++;
    }
    return (int)(d - dst);
}

static str_t xml_unescape(const char *s, int len)
{
    str_t out;
    if (len <= 0 || memchr(s, '&', (size_t)len) == NULL) {
        out.p = s;
        out.n = len < 0 ? 0 : len;
        return out;
    }
    char *buf = R_alloc((size_t)len, 1);
    out.n = unescape_into(s, len, buf);
    out.p = buf;
    return out;
}

/* next <t> text run inside [p,end); skips phonetic <rPh> blocks */
static const char *next_text_run(const char *p, const char *end, str_t *run)
{
    while (p < end) {
        p = memchr(p, '<', (size_t)(end - p));
        if (!p || p + 1 >= end) return NULL;
        if (p[1] == 't' && (p + 2 >= end || p[2] == '>' || p[2] == ' ' || p[2] == '/')) {
            int self;
            const char *c = tag_close(p + 2, end, &self);
            if (self) { run->p = c; run->n = 0; return c; }
            const char *stop = sfind(c, end, "</t>");
            if (!stop) return NULL;
            run->p = c;
            run->n = (int)(stop - c);
            return stop + 4;
        }
        if (end - p >= 4 && memcmp(p, "<rPh", 4) == 0) {
            const char *stop = sfind(p, end, "</rPh>");
            p = stop ? stop + 6 : end;
            continue;
        }
        p++;
    }
    return NULL;
}

/* concatenated, unescaped text content of an <si> or <is> region */
static str_t collect_text(const char *p, const char *end)
{
    str_t out = {NULL, 0};
    str_t run;
    const char *after1 = next_text_run(p, end, &run);
    if (!after1) return out;
    str_t run2;
    const char *after2 = next_text_run(after1, end, &run2);
    if (!after2)
        return xml_unescape(run.p, run.n);
    char *buf = R_alloc((size_t)(end - p), 1);
    int used = unescape_into(run.p, run.n, buf);
    used += unescape_into(run2.p, run2.n, buf + used);
    const char *cur = after2;
    while ((cur = next_text_run(cur, end, &run)) != NULL)
        used += unescape_into(run.p, run.n, buf + used);
    out.p = buf;
    out.n = used;
    return out;
}


static int zread(mz_zip_archive *za, const char *name, buf_t *out)
{
    int idx = mz_zip_reader_locate_file(za, name, NULL, 0);
    if (idx < 0) return 0;
    mz_zip_archive_file_stat st;
    if (!mz_zip_reader_file_stat(za, (mz_uint)idx, &st)) return 0;
    char *buf = R_alloc((size_t)st.m_uncomp_size + 1, 1);
    if (!mz_zip_reader_extract_to_mem(za, (mz_uint)idx, buf, (size_t)st.m_uncomp_size, 0))
        return 0;
    buf[st.m_uncomp_size] = 0;
    out->p = buf;
    out->n = (size_t)st.m_uncomp_size;
    return 1;
}


static int is_builtin_date(long id)
{
    return (id >= 14 && id <= 22) || (id >= 27 && id <= 36) ||
           (id >= 45 && id <= 47) || (id >= 50 && id <= 58);
}

static int fmt_code_is_date(const char *s, int len)
{
    const char *end = s + len;
    while (s < end) {
        char c = *s;
        if (c == '"') {
            s++;
            while (s < end && *s != '"') s++;
            if (s < end) s++;
            continue;
        }
        if (c == '\\') { s += 2; continue; }
        if (c == '[') {
            /* elapsed-time codes like [h] [mm] [ss] are date formats */
            if (s + 1 < end) {
                char b = s[1];
                if (b == 'h' || b == 'H' || b == 'm' || b == 'M' || b == 's' || b == 'S')
                    return 1;
            }
            s++;
            while (s < end && *s != ']') s++;
            if (s < end) s++;
            continue;
        }
        if (c == 'y' || c == 'Y' || c == 'd' || c == 'D' ||
            c == 'h' || c == 'H' || c == 's' || c == 'S' ||
            c == 'm' || c == 'M')
            return 1;
        s++;
    }
    return 0;
}

static void parse_styles(buf_t sty, unsigned char **xf_date_out, int *n_xf_out)
{
    *xf_date_out = NULL;
    *n_xf_out = 0;
    if (!sty.p) return;
    const char *end = sty.p + sty.n;

    long *date_ids = NULL;
    int n_date_ids = 0, cap_date_ids = 0;
    const char *p = sty.p;
    const char *nf_end = end;
    const char *nf = sfind(p, end, "<numFmts");
    if (nf) {
        const char *stop = sfind(nf, end, "</numFmts>");
        if (stop) nf_end = stop;
        p = nf;
        while ((p = find_tag(p, nf_end, "<numFmt")) != NULL) {
            p += 7;
            long id = -1;
            str_t code = {NULL, 0};
            attr_t a;
            const char *q = p;
            const char *q2;
            while ((q2 = next_attr(q, nf_end, &a)) != NULL) {
                if (attr_is(&a, "numFmtId")) id = slice_long(a.v, a.vlen);
                else if (attr_is(&a, "formatCode")) { code.p = a.v; code.n = a.vlen; }
                q = q2;
            }
            if (id >= 0 && code.p) {
                str_t dec = xml_unescape(code.p, code.n);
                if (fmt_code_is_date(dec.p, dec.n)) {
                    if (n_date_ids == cap_date_ids) {
                        int nc = cap_date_ids ? cap_date_ids * 2 : 16;
                        long *nd = (long *)R_alloc((size_t)nc, sizeof(long));
                        if (date_ids) memcpy(nd, date_ids, (size_t)n_date_ids * sizeof(long));
                        date_ids = nd;
                        cap_date_ids = nc;
                    }
                    date_ids[n_date_ids++] = id;
                }
            }
        }
    }

    const char *xfs = sfind(sty.p, end, "<cellXfs");
    if (!xfs) return;
    const char *xfs_end = sfind(xfs, end, "</cellXfs>");
    if (!xfs_end) xfs_end = end;

    int n_xf = 0, cap_xf = 64;
    unsigned char *xf_date = (unsigned char *)R_alloc((size_t)cap_xf, 1);
    p = xfs;
    while ((p = find_tag(p, xfs_end, "<xf")) != NULL) {
        p += 3;
        long id = 0;
        attr_t a;
        const char *q = p;
        const char *q2;
        while ((q2 = next_attr(q, xfs_end, &a)) != NULL) {
            if (attr_is(&a, "numFmtId")) id = slice_long(a.v, a.vlen);
            q = q2;
        }
        int isdate = is_builtin_date(id);
        if (!isdate)
            for (int i = 0; i < n_date_ids; i++)
                if (date_ids[i] == id) { isdate = 1; break; }
        if (n_xf == cap_xf) {
            unsigned char *nx = (unsigned char *)R_alloc((size_t)cap_xf * 2, 1);
            memcpy(nx, xf_date, (size_t)n_xf);
            xf_date = nx;
            cap_xf *= 2;
        }
        xf_date[n_xf++] = (unsigned char)isdate;
    }
    *xf_date_out = xf_date;
    *n_xf_out = n_xf;
}


static void parse_sst(buf_t sst, str_t **out, long *n_out)
{
    *out = NULL;
    *n_out = 0;
    if (!sst.p) return;
    const char *end = sst.p + sst.n;
    long cap = 0;
    const char *hdr = find_tag(sst.p, end, "<sst");
    if (hdr) {
        attr_t a;
        const char *q = hdr + 4;
        const char *q2;
        while ((q2 = next_attr(q, end, &a)) != NULL) {
            if (attr_is(&a, "uniqueCount")) cap = slice_long(a.v, a.vlen);
            q = q2;
        }
    }
    if (cap <= 0) cap = 1024;
    str_t *arr = (str_t *)R_alloc((size_t)cap, sizeof(str_t));
    long n = 0;
    const char *p = sst.p;
    while ((p = find_tag(p, end, "<si")) != NULL) {
        int self;
        const char *body = tag_close(p + 3, end, &self);
        const char *stop = self ? body : sfind(body, end, "</si>");
        if (!stop) stop = end;
        if (n == cap) {
            str_t *na = (str_t *)R_alloc((size_t)cap * 2, sizeof(str_t));
            memcpy(na, arr, (size_t)n * sizeof(str_t));
            arr = na;
            cap *= 2;
        }
        if (self) {
            arr[n].p = NULL;
            arr[n].n = 0;
        } else
            arr[n] = collect_text(body, stop);
        n++;
        p = stop;
    }
    *out = arr;
    *n_out = n;
}


static long wb_find_sheet(buf_t wb, SEXP sheetArg, char *rid, size_t rid_cap,
                          str_t *name_out)
{
    const char *end = wb.p + wb.n;
    const char *p = wb.p;
    const char *want_name = NULL;
    long want_idx = -1;
    if (TYPEOF(sheetArg) == STRSXP)
        want_name = translateCharUTF8(STRING_ELT(sheetArg, 0));
    else
        want_idx = asInteger(sheetArg);

    long idx = 0;
    while ((p = find_tag(p, end, "<sheet")) != NULL) {
        p += 6;
        str_t nm = {NULL, 0};
        str_t r = {NULL, 0};
        attr_t a;
        const char *q = p;
        const char *q2;
        while ((q2 = next_attr(q, end, &a)) != NULL) {
            if (attr_is(&a, "name")) { nm.p = a.v; nm.n = a.vlen; }
            else if (attr_is(&a, "r:id")) { r.p = a.v; r.n = a.vlen; }
            q = q2;
        }
        idx++;
        int match;
        if (want_name) {
            str_t dec = xml_unescape(nm.p, nm.n);
            match = (size_t)dec.n == strlen(want_name) &&
                    memcmp(dec.p, want_name, (size_t)dec.n) == 0;
        } else
            match = idx == want_idx;
        if (match) {
            if (r.p && (size_t)r.n < rid_cap) {
                memcpy(rid, r.p, (size_t)r.n);
                rid[r.n] = 0;
            } else
                rid[0] = 0;
            *name_out = nm;
            return idx;
        }
    }
    return -1;
}

static int wb_date1904(buf_t wb)
{
    const char *end = wb.p + wb.n;
    const char *p = find_tag(wb.p, end, "<workbookPr");
    if (!p) return 0;
    attr_t a;
    const char *q = p + 11;
    const char *q2;
    while ((q2 = next_attr(q, end, &a)) != NULL) {
        if (attr_is(&a, "date1904"))
            return a.vlen > 0 && (a.v[0] == '1' || a.v[0] == 't' || a.v[0] == 'T');
        q = q2;
    }
    return 0;
}

static int rels_target(buf_t rels, const char *rid, char *out, size_t out_cap)
{
    if (!rels.p || !rid[0]) return 0;
    const char *end = rels.p + rels.n;
    const char *p = rels.p;
    size_t rid_len = strlen(rid);
    while ((p = find_tag(p, end, "<Relationship")) != NULL) {
        p += 13;
        str_t id = {NULL, 0};
        str_t tgt = {NULL, 0};
        attr_t a;
        const char *q = p;
        const char *q2;
        while ((q2 = next_attr(q, end, &a)) != NULL) {
            if (attr_is(&a, "Id")) { id.p = a.v; id.n = a.vlen; }
            else if (attr_is(&a, "Target")) { tgt.p = a.v; tgt.n = a.vlen; }
            q = q2;
        }
        if (id.p && (size_t)id.n == rid_len && memcmp(id.p, rid, rid_len) == 0 && tgt.p) {
            str_t dec = xml_unescape(tgt.p, tgt.n);
            if (dec.n > 0 && dec.p[0] == '/') {
                if ((size_t)dec.n - 1 >= out_cap) return 0;
                memcpy(out, dec.p + 1, (size_t)dec.n - 1);
                out[dec.n - 1] = 0;
            } else {
                if ((size_t)dec.n + 3 >= out_cap) return 0;
                memcpy(out, "xl/", 3);
                memcpy(out + 3, dec.p, (size_t)dec.n);
                out[dec.n + 3] = 0;
            }
            return 1;
        }
    }
    return 0;
}


enum {
    CELL_BLANK = 0,
    CELL_NUM,
    CELL_DATE,
    CELL_SST,
    CELL_STR,
    CELL_BOOL,
    CELL_EMPTY   /* had a value that is empty/whitespace-only text: counts
                    for sheet extent, but is NA and never affects typing */
};

typedef struct {
    unsigned char *tag;
    double *num;
    str_t *str;      /* lazily allocated; only for CELL_STR */
    R_xlen_t cap;
} col_t;

typedef struct {
    col_t *cols;
    int ncols, ncols_cap;
    R_xlen_t nrow;
    R_xlen_t row_cap_hint;
} grid_t;

static void grid_ensure_col(grid_t *g, int col)
{
    if (col < g->ncols) return;
    if (col >= g->ncols_cap) {
        int nc = g->ncols_cap ? g->ncols_cap : 16;
        while (nc <= col) nc *= 2;
        col_t *na = (col_t *)R_alloc((size_t)nc, sizeof(col_t));
        if (g->cols) memcpy(na, g->cols, (size_t)g->ncols * sizeof(col_t));
        memset(na + g->ncols, 0, (size_t)(nc - g->ncols) * sizeof(col_t));
        g->cols = na;
        g->ncols_cap = nc;
    }
    g->ncols = col + 1;
}

static void col_ensure_row(col_t *c, R_xlen_t row, R_xlen_t hint)
{
    if (row < c->cap) return;
    R_xlen_t nc = c->cap ? c->cap * 2 : (hint > 0 ? hint : 256);
    while (nc <= row) nc *= 2;
    unsigned char *tag = (unsigned char *)R_alloc((size_t)nc, 1);
    double *num = (double *)R_alloc((size_t)nc, sizeof(double));
    memset(tag, 0, (size_t)nc);
    if (c->tag) {
        memcpy(tag, c->tag, (size_t)c->cap);
        memcpy(num, c->num, (size_t)c->cap * sizeof(double));
    }
    if (c->str) {
        str_t *str = (str_t *)R_alloc((size_t)nc, sizeof(str_t));
        memcpy(str, c->str, (size_t)c->cap * sizeof(str_t));
        c->str = str;
    }
    c->tag = tag;
    c->num = num;
    c->cap = nc;
}

static str_t str_trim(str_t s)
{
    const char *p = s.p;
    const char *e = s.p + s.n;
    while (p < e && (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n')) p++;
    while (e > p && (e[-1] == ' ' || e[-1] == '\t' || e[-1] == '\r' || e[-1] == '\n')) e--;
    s.p = p;
    s.n = (int)(e - p);
    return s;
}

static void col_set_str(col_t *c, R_xlen_t row, str_t s)
{
    /* readxl semantics, independent of trim_ws: empty and whitespace-only
       strings read as NA and don't influence typing, but the cell still
       counts toward the sheet extent */
    if (str_trim(s).n == 0) { c->tag[row] = CELL_EMPTY; return; }
    if (!c->str)
        c->str = (str_t *)R_alloc((size_t)c->cap, sizeof(str_t));
    c->tag[row] = CELL_STR;
    c->str[row] = s;
}

static unsigned char cell_tag(const col_t *c, R_xlen_t row)
{
    return row < c->cap ? c->tag[row] : (unsigned char)CELL_BLANK;
}

static void parse_sheet(buf_t sheet, grid_t *g, const unsigned char *xf_date, int n_xf)
{
    const char *end = sheet.p + sheet.n;
    const char *p;

    p = find_tag(sheet.p, end, "<dimension");
    if (p) {
        attr_t a;
        const char *q = p + 10;
        const char *q2;
        while ((q2 = next_attr(q, end, &a)) != NULL) {
            if (attr_is(&a, "ref")) {
                const char *colon = memchr(a.v, ':', (size_t)a.vlen);
                if (colon) {
                    const char *r2 = colon + 1;
                    int len = (int)(a.v + a.vlen - r2);
                    int i = 0;
                    while (i < len && !(r2[i] >= '0' && r2[i] <= '9')) i++;
                    g->row_cap_hint = slice_long(r2 + i, len - i);
                    int c = ref_col(r2, len);
                    if (c >= 0 && c < 16384) grid_ensure_col(g, c);
                }
            }
            q = q2;
        }
    }

    long cur_row = -1;
    p = sheet.p;
    for (;;) {
        p = find_tag(p, end, "<row");
        if (!p) break;
        int self;
        long rnum = -1;
        attr_t a;
        const char *q = p + 4;
        const char *q2;
        while ((q2 = next_attr(q, end, &a)) != NULL) {
            if (attr_is(&a, "r")) rnum = slice_long(a.v, a.vlen);
            q = q2;
        }
        p = tag_close(q, end, &self);
        cur_row = rnum > 0 ? rnum - 1 : cur_row + 1;
        R_xlen_t row = (R_xlen_t)cur_row;
        if (row + 1 > g->nrow) g->nrow = row + 1;
        if (self) continue;

        int last_col = -1;
        while (p < end) {
            p = memchr(p, '<', (size_t)(end - p));
            if (!p) { p = end; break; }
            if (p + 1 >= end) { p = end; break; }
            if (p[1] == '/' && end - p >= 6 && memcmp(p + 2, "row>", 4) == 0) {
                p += 6;
                break;
            }
            if (end - p >= 4 && p[1] == 'r' && p[2] == 'o' && p[3] == 'w' &&
                (p[4] == ' ' || p[4] == '>' || p[4] == '/'))
                break; /* malformed: next row without </row> */
            if (!(p[1] == 'c' &&
                  (p + 2 >= end || p[2] == ' ' || p[2] == '>' || p[2] == '/'))) {
                p++;
                continue;
            }

            int col = -1;
            long style = -1;
            int t = 'n';        /* n, s(shared), r(str/raw), b, e, i(inline), d(iso) */
            q = p + 2;
            while ((q2 = next_attr(q, end, &a)) != NULL) {
                if (attr_is(&a, "r")) col = ref_col(a.v, a.vlen);
                else if (attr_is(&a, "s")) style = slice_long(a.v, a.vlen);
                else if (attr_is(&a, "t")) {
                    if (a.vlen == 1) {
                        if (a.v[0] == 's') t = 's';
                        else if (a.v[0] == 'b') t = 'b';
                        else if (a.v[0] == 'e') t = 'e';
                        else if (a.v[0] == 'd') t = 'd';
                        else if (a.v[0] == 'n') t = 'n';
                    } else if (a.vlen == 3 && memcmp(a.v, "str", 3) == 0) t = 'r';
                    else if (a.vlen == 9 && memcmp(a.v, "inlineStr", 9) == 0) t = 'i';
                }
                q = q2;
            }
            p = tag_close(q, end, &self);
            if (col < 0) col = last_col + 1;
            last_col = col;
            if (col >= 16384) continue;
            grid_ensure_col(g, col);
            col_t *c = &g->cols[col];
            col_ensure_row(c, row, g->row_cap_hint);
            if (self) continue;

            const char *cend = sfind(p, end, "</c>");
            if (!cend) cend = end;

            if (t == 'i') {
                const char *is = find_tag(p, cend, "<is");
                if (is) {
                    int s2;
                    const char *body = tag_close(is + 3, cend, &s2);
                    if (!s2) col_set_str(c, row, collect_text(body, cend));
                }
            } else if (t != 'e') {
                const char *v = find_tag(p, cend, "<v");
                if (v) {
                    int s2;
                    const char *vs = tag_close(v + 2, cend, &s2);
                    if (!s2) {
                        const char *ve = sfind(vs, cend, "</v>");
                        if (!ve) ve = cend;
                        int vlen = (int)(ve - vs);
                        if (t == 's') {
                            c->tag[row] = CELL_SST;
                            c->num[row] = (double)slice_long(vs, vlen);
                        } else if (t == 'b') {
                            c->tag[row] = CELL_BOOL;
                            c->num[row] = (vlen > 0 && (vs[0] == '1' || vs[0] == 't'))
                                              ? 1.0 : 0.0;
                        } else if (t == 'r' || t == 'd') {
                            col_set_str(c, row, xml_unescape(vs, vlen));
                        } else if (vlen > 0 && vlen < 64) {
                            char nbuf[64];
                            memcpy(nbuf, vs, (size_t)vlen);
                            nbuf[vlen] = 0;
                            char *ep = NULL;
                            /* strtod, not R_strtod: correctly rounded, so
                               values agree bit-for-bit with other parsers */
                            double d = strtod(nbuf, &ep);
                            if (ep && *ep == 0) {
                                int isdate = style >= 0 && style < n_xf &&
                                             xf_date[style];
                                c->tag[row] = isdate ? CELL_DATE : CELL_NUM;
                                c->num[row] = d;
                            } else
                                col_set_str(c, row, xml_unescape(vs, vlen));
                        } else if (vlen > 0)
                            col_set_str(c, row, xml_unescape(vs, vlen));
                    }
                }
            }
            p = cend < end ? cend + 4 : end;
        }
    }
}


static SEXP cell_charsxp(const col_t *c, R_xlen_t row, SEXP sst_table, long n_sst,
                         int trim)
{
    char tmp[32];
    switch (cell_tag(c, row)) {
    case CELL_SST: {
        long i = (long)c->num[row];
        if (i >= 0 && i < n_sst) return STRING_ELT(sst_table, i);
        return NA_STRING;
    }
    case CELL_STR: {
        str_t s = trim ? str_trim(c->str[row]) : c->str[row];
        return mkCharLenCE(s.p, s.n, CE_UTF8);
    }
    case CELL_NUM:
    case CELL_DATE:
        snprintf(tmp, sizeof tmp, "%.15g", c->num[row]);
        return mkCharLenCE(tmp, (int)strlen(tmp), CE_UTF8);
    case CELL_BOOL:
        return mkCharLenCE(c->num[row] != 0.0 ? "TRUE" : "FALSE",
                           c->num[row] != 0.0 ? 4 : 5, CE_UTF8);
    default:
        return NA_STRING;
    }
}

SEXP C_read_xlsx(SEXP path, SEXP sheetArg, SEXP colNamesArg, SEXP trimArg)
{
    const char *cpath = translateChar(STRING_ELT(path, 0));
    int want_names = asLogical(colNamesArg) == TRUE;
    int trim = asLogical(trimArg) == TRUE;

    mz_zip_archive za;
    memset(&za, 0, sizeof za);
    if (!mz_zip_reader_init_file(&za, cpath, 0))
        error("cannot open '%s' as a zip archive", cpath);

    buf_t wb = {NULL, 0}, rels = {NULL, 0}, sstbuf = {NULL, 0},
          sty = {NULL, 0}, sheet = {NULL, 0};
    if (!zread(&za, "xl/workbook.xml", &wb)) {
        mz_zip_reader_end(&za);
        error("'%s' has no xl/workbook.xml; not an xlsx file", cpath);
    }
    zread(&za, "xl/_rels/workbook.xml.rels", &rels);
    zread(&za, "xl/sharedStrings.xml", &sstbuf);
    zread(&za, "xl/styles.xml", &sty);

    char rid[64];
    str_t sheet_name = {NULL, 0};
    long sheet_idx = wb_find_sheet(wb, sheetArg, rid, sizeof rid, &sheet_name);
    if (sheet_idx < 0) {
        mz_zip_reader_end(&za);
        if (TYPEOF(sheetArg) == STRSXP)
            error("sheet '%s' not found", translateCharUTF8(STRING_ELT(sheetArg, 0)));
        error("sheet %d not found", asInteger(sheetArg));
    }

    char target[512];
    if (!rels_target(rels, rid, target, sizeof target))
        snprintf(target, sizeof target, "xl/worksheets/sheet%ld.xml", sheet_idx);
    if (!zread(&za, target, &sheet)) {
        mz_zip_reader_end(&za);
        error("cannot extract worksheet part '%s'", target);
    }
    int date1904 = wb_date1904(wb);
    mz_zip_reader_end(&za);

    unsigned char *xf_date = NULL;
    int n_xf = 0;
    parse_styles(sty, &xf_date, &n_xf);

    str_t *sst = NULL;
    long n_sst = 0;
    parse_sst(sstbuf, &sst, &n_sst);

    grid_t g;
    memset(&g, 0, sizeof g);
    parse_sheet(sheet, &g, xf_date, n_xf);

    /* cells referencing empty/whitespace-only shared strings behave like
       inline blanks; out-of-range references are treated the same way */
    unsigned char *sst_blank =
        n_sst ? (unsigned char *)R_alloc((size_t)n_sst, 1) : NULL;
    for (long i = 0; i < n_sst; i++)
        sst_blank[i] = !sst[i].p || str_trim(sst[i]).n == 0;
    for (int j = 0; j < g.ncols; j++) {
        col_t *c = &g.cols[j];
        R_xlen_t rmax = c->cap < g.nrow ? c->cap : g.nrow;
        for (R_xlen_t r = 0; r < rmax; r++)
            if (c->tag[r] == CELL_SST) {
                long i = (long)c->num[r];
                if (i < 0 || i >= n_sst || sst_blank[i])
                    c->tag[r] = CELL_EMPTY;
            }
    }

    SEXP sst_table = PROTECT(allocVector(STRSXP, n_sst));
    for (long i = 0; i < n_sst; i++) {
        str_t s = sst[i];
        if (trim && s.p) s = str_trim(s);
        SET_STRING_ELT(sst_table, i,
                       s.p ? mkCharLenCE(s.p, s.n, CE_UTF8) : mkCharLen("", 0));
    }

    /* trim fully-blank edge rows and columns */
    R_xlen_t r0 = 0, r1 = g.nrow;
    int c0 = 0, c1 = g.ncols;
    {
        int found = 0;
        for (; r0 < g.nrow && !found; )
        {
            for (int j = 0; j < g.ncols; j++)
                if (cell_tag(&g.cols[j], r0) != CELL_BLANK) { found = 1; break; }
            if (!found) r0++;
        }
        found = 0;
        while (r1 > r0 && !found) {
            for (int j = 0; j < g.ncols; j++)
                if (cell_tag(&g.cols[j], r1 - 1) != CELL_BLANK) { found = 1; break; }
            if (!found) r1--;
        }
        found = 0;
        while (c0 < g.ncols && !found) {
            for (R_xlen_t r = r0; r < r1; r++)
                if (cell_tag(&g.cols[c0], r) != CELL_BLANK) { found = 1; break; }
            if (!found) c0++;
        }
        found = 0;
        while (c1 > c0 && !found) {
            for (R_xlen_t r = r0; r < r1; r++)
                if (cell_tag(&g.cols[c1 - 1], r) != CELL_BLANK) { found = 1; break; }
            if (!found) c1--;
        }
    }

    R_xlen_t data0 = want_names && r1 > r0 ? r0 + 1 : r0;
    R_xlen_t n = r1 - data0;
    int nc = c1 - c0;
    if (nc < 0) nc = 0;
    if (n < 0) n = 0;

    SEXP ans = PROTECT(allocVector(VECSXP, nc));
    SEXP nms = PROTECT(allocVector(STRSXP, nc));
    double epoch = date1904 ? 24107.0 : 25569.0;

    for (int j = 0; j < nc; j++) {
        col_t *c = &g.cols[c0 + j];
        R_xlen_t nnum = 0, ndate = 0, nbool = 0, nstr = 0;
        for (R_xlen_t r = data0; r < r1; r++) {
            switch (cell_tag(c, r)) {
            case CELL_NUM:  nnum++; break;
            case CELL_DATE: ndate++; break;
            case CELL_BOOL: nbool++; break;
            case CELL_SST:
            case CELL_STR:  nstr++; break;
            default: break;
            }
        }

        SEXP col;
        if (nstr > 0) {
            col = allocVector(STRSXP, n);
            SET_VECTOR_ELT(ans, j, col);
            for (R_xlen_t r = data0; r < r1; r++)
                SET_STRING_ELT(col, r - data0,
                               cell_charsxp(c, r, sst_table, n_sst, trim));
        } else if (nbool > 0 && nnum == 0 && ndate == 0) {
            col = allocVector(LGLSXP, n);
            SET_VECTOR_ELT(ans, j, col);
            int *lp = LOGICAL(col);
            for (R_xlen_t r = data0; r < r1; r++)
                lp[r - data0] = cell_tag(c, r) == CELL_BOOL
                                    ? (c->num[r] != 0.0) : NA_LOGICAL;
        } else if (ndate > 0 && nnum == 0) {
            col = allocVector(REALSXP, n);
            SET_VECTOR_ELT(ans, j, col);
            double *dp = REAL(col);
            for (R_xlen_t r = data0; r < r1; r++) {
                if (cell_tag(c, r) != CELL_DATE) {
                    dp[r - data0] = NA_REAL;
                    continue;
                }
                double serial = c->num[r];
                /* Excel's 1900 system counts a nonexistent 1900-02-29;
                   serials before it are one day behind the real calendar. */
                if (!date1904 && serial < 61.0) serial += 1.0;
                dp[r - data0] = (serial - epoch) * 86400.0;
            }
            SEXP kl = PROTECT(allocVector(STRSXP, 2));
            SET_STRING_ELT(kl, 0, mkChar("POSIXct"));
            SET_STRING_ELT(kl, 1, mkChar("POSIXt"));
            setAttrib(col, R_ClassSymbol, kl);
            setAttrib(col, install("tzone"), mkString("UTC"));
            UNPROTECT(1);
        } else if (nnum > 0 || ndate > 0 || nbool > 0) {
            col = allocVector(REALSXP, n);
            SET_VECTOR_ELT(ans, j, col);
            double *dp = REAL(col);
            for (R_xlen_t r = data0; r < r1; r++) {
                unsigned char tg = cell_tag(c, r);
                dp[r - data0] = (tg == CELL_NUM || tg == CELL_DATE ||
                                 tg == CELL_BOOL)
                                    ? c->num[r] : NA_REAL;
            }
        } else {
            col = allocVector(LGLSXP, n);
            SET_VECTOR_ELT(ans, j, col);
            int *lp = LOGICAL(col);
            for (R_xlen_t r = 0; r < n; r++) lp[r] = NA_LOGICAL;
        }

        if (want_names && r1 > r0) {
            SEXP nm = cell_charsxp(&g.cols[c0 + j], r0, sst_table, n_sst, trim);
            if (nm == NA_STRING) {
                char tmp[16];
                snprintf(tmp, sizeof tmp, "V%d", j + 1);
                nm = mkCharLenCE(tmp, (int)strlen(tmp), CE_UTF8);
            }
            SET_STRING_ELT(nms, j, nm);
        } else {
            char tmp[16];
            snprintf(tmp, sizeof tmp, "V%d", j + 1);
            SET_STRING_ELT(nms, j, mkCharLenCE(tmp, (int)strlen(tmp), CE_UTF8));
        }
    }

    setAttrib(ans, R_NamesSymbol, nms);
    UNPROTECT(3);
    return ans;
}

SEXP C_sheet_names(SEXP path)
{
    const char *cpath = translateChar(STRING_ELT(path, 0));
    mz_zip_archive za;
    memset(&za, 0, sizeof za);
    if (!mz_zip_reader_init_file(&za, cpath, 0))
        error("cannot open '%s' as a zip archive", cpath);
    buf_t wb = {NULL, 0};
    if (!zread(&za, "xl/workbook.xml", &wb)) {
        mz_zip_reader_end(&za);
        error("'%s' has no xl/workbook.xml; not an xlsx file", cpath);
    }
    mz_zip_reader_end(&za);

    const char *end = wb.p + wb.n;
    long count = 0;
    const char *p = wb.p;
    while ((p = find_tag(p, end, "<sheet")) != NULL) { count++; p += 6; }

    SEXP ans = PROTECT(allocVector(STRSXP, count));
    p = wb.p;
    long i = 0;
    while ((p = find_tag(p, end, "<sheet")) != NULL) {
        p += 6;
        attr_t a;
        const char *q = p;
        const char *q2;
        str_t nm = {NULL, 0};
        while ((q2 = next_attr(q, end, &a)) != NULL) {
            if (attr_is(&a, "name")) { nm.p = a.v; nm.n = a.vlen; }
            q = q2;
        }
        str_t dec = xml_unescape(nm.p, nm.n);
        SET_STRING_ELT(ans, i++, mkCharLenCE(dec.p, dec.n, CE_UTF8));
    }
    UNPROTECT(1);
    return ans;
}
