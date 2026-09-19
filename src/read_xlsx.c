#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <stdint.h>
#include <math.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>
#include "miniz.h"
#include "libdeflate.h"
#include "xthread.h"


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

/* Attribute digits come from an untrusted archive, so an absurdly long run
   must not overflow: saturate instead, and let the callers' range checks
   reject the value the way they reject any other out-of-range one. */
static long slice_long(const char *s, int len)
{
    long v = 0;
    int i = 0;
    for (; i < len && s[i] >= '0' && s[i] <= '9'; i++) {
        int d = s[i] - '0';
        if (v > (LONG_MAX - d) / 10) return LONG_MAX;
        v = v * 10 + d;
    }
    return v;
}

/* column part of a cell ref like "BC12" -> 0-based index */
static int ref_col(const char *s, int len)
{
    int c = 0, i;
    for (i = 0; i < len; i++) {
        char ch = s[i];
        int d;
        if (ch >= 'A' && ch <= 'Z') d = ch - 'A' + 1;
        else if (ch >= 'a' && ch <= 'z') d = ch - 'a' + 1;
        else break;
        if (c > (INT_MAX - d) / 26) { c = INT_MAX; break; }
        c = c * 26 + d;
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

/* Worksheet text is declared UTF-8 by the format, but nothing enforces it.
   Handing R a CE_UTF8 string with invalid bytes makes nchar(), substr() and
   toupper() throw on the returned data frame, so bad bytes are repaired
   rather than passed through. */
static int utf8_seq_len(const unsigned char *p, const unsigned char *e,
                        unsigned int *cp)
{
    unsigned char c = *p;
    int len;
    if (c < 0x80) { *cp = c; return 1; }
    if ((c & 0xE0) == 0xC0) { len = 2; *cp = c & 0x1Fu; }
    else if ((c & 0xF0) == 0xE0) { len = 3; *cp = c & 0x0Fu; }
    else if ((c & 0xF8) == 0xF0) { len = 4; *cp = c & 0x07u; }
    else return 0;
    if (e - p < len) return 0;
    for (int i = 1; i < len; i++) {
        if ((p[i] & 0xC0) != 0x80) return 0;
        *cp = (*cp << 6) | (unsigned)(p[i] & 0x3F);
    }
    if (*cp > 0x10FFFF || (*cp >= 0xD800 && *cp <= 0xDFFF)) return 0;
    /* overlong encodings decode to a valid scalar but are not valid UTF-8 */
    if ((len == 2 && *cp < 0x80) || (len == 3 && *cp < 0x800) ||
        (len == 4 && *cp < 0x10000)) return 0;
    return len;
}

static int utf8_valid(const char *s, int n)
{
    const unsigned char *p = (const unsigned char *)s, *e = p + n;
    while (p < e) {
        /* worksheet text is overwhelmingly ASCII: clear it eight bytes at a
           time and only decode where a high bit actually appears */
        while (e - p >= 8) {
            uint64_t w;
            memcpy(&w, p, 8);
            if (w & 0x8080808080808080ULL) break;
            p += 8;
        }
        if (p >= e) break;
        if (*p < 0x80) { p++; continue; }
        unsigned int cp;
        int len = utf8_seq_len(p, e, &cp);
        if (!len) return 0;
        p += len;
    }
    return 1;
}

/* each invalid byte becomes U+FFFD; only reached when utf8_valid fails */
static str_t utf8_sanitize(const char *s, int n)
{
    str_t out;
    char *buf = R_alloc((size_t)n * 3 + 1, 1);
    char *d = buf;
    const unsigned char *p = (const unsigned char *)s, *e = p + n;
    while (p < e) {
        unsigned int cp;
        int len = utf8_seq_len(p, e, &cp);
        if (!len) { d += utf8_put(0xFFFD, d); p++; continue; }
        memcpy(d, p, (size_t)len);
        d += len;
        p += len;
    }
    out.p = buf;
    out.n = (int)(d - buf);
    return out;
}

/* R string from worksheet bytes, repairing invalid UTF-8 */
static SEXP mk_utf8(const char *p, int n)
{
    if (n <= 0) return mkCharLenCE(p ? p : "", 0, CE_UTF8);
    if (utf8_valid(p, n)) return mkCharLenCE(p, n, CE_UTF8);
    str_t s = utf8_sanitize(p, n);
    return mkCharLenCE(s.p, s.n, CE_UTF8);
}

/* Excel serial -> seconds since the R epoch.  The 1900 system counts a
   nonexistent 1900-02-29, so serials before it run a day behind the real
   calendar.  The day fraction is a base-10 value with no exact binary form,
   so snap to the nearest millisecond: left alone, a whole second arrives as
   ...:55.999999 and formats one second early. */
static double serial_seconds(double serial, double epoch, int date1904)
{
    if (!date1904 && serial < 61.0) serial += 1.0;
    return round((serial - epoch) * 86400.0 * 1000.0) / 1000.0;
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
            int ndig = 0, ok = 1;
            if (*q == 'x' || *q == 'X') {
                q++;
                while (q < end && *q != ';' && ok) {
                    char c = *q;
                    cp <<= 4;
                    if (c >= '0' && c <= '9') cp |= (unsigned)(c - '0');
                    else if (c >= 'a' && c <= 'f') cp |= (unsigned)(c - 'a' + 10);
                    else if (c >= 'A' && c <= 'F') cp |= (unsigned)(c - 'A' + 10);
                    else ok = 0;
                    if (ok && ++ndig > 6) ok = 0;
                    if (ok) q++;
                }
            } else {
                while (q < end && *q != ';' && ok) {
                    if (*q >= '0' && *q <= '9') cp = cp * 10 + (unsigned)(*q - '0');
                    else ok = 0;
                    if (ok && ++ndig > 7) ok = 0;
                    if (ok) q++;
                }
            }
            /* a character reference must terminate and name a real scalar
               value; a malformed one is literal text, not a NUL byte */
            if (!ok || ndig == 0 || q >= end || *q != ';' || cp == 0 ||
                cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF)) {
                *d++ = *s++;
            } else {
                d += utf8_put(cp, d);
                s = q + 1;
            }
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


/* Persistent scratch arena for the large transient buffers (file bytes,
   decompressed parts, grid arrays).  R_alloc would hand the pages back to
   the OS after every call, so each read would re-fault its whole working
   set; these malloc blocks are kept across calls and reused warm.  Entered
   only from the single R thread (workers never allocate), reset on entry,
   and trimmed so at most ~256MB stays cached.  A longjmp from error()
   cannot leak blocks: they stay owned by the static list. */
typedef struct sblock {
    struct sblock *next;
    size_t cap, used;
} sblock_t;

static sblock_t *g_scratch;

static void scratch_reset(void)
{
    size_t total = 0;
    sblock_t **pb = &g_scratch;
    while (*pb) {
        sblock_t *b = *pb;
        total += b->cap;
        if (total > ((size_t)256 << 20)) {
            *pb = b->next;
            free(b);
            continue;
        }
        b->used = 0;
        pb = &b->next;
    }
}

static void *scratch_alloc(size_t n)
{
    n = (n + 15u) & ~(size_t)15;
    for (sblock_t *b = g_scratch; b; b = b->next)
        if (b->cap - b->used >= n) {
            void *p = (char *)(b + 1) + b->used;
            b->used += n;
            return p;
        }
    size_t cap = g_scratch && g_scratch->cap >= n ? g_scratch->cap * 2 : n;
    if (cap < ((size_t)1 << 20)) cap = (size_t)1 << 20;
    sblock_t *b = (sblock_t *)malloc(sizeof(sblock_t) + cap);
    if (!b && cap > n) b = (sblock_t *)malloc(sizeof(sblock_t) + (cap = n));
    if (!b) error("cannot allocate %lu bytes", (unsigned long)n);
    b->cap = cap;
    b->used = n;
    b->next = g_scratch;
    g_scratch = b;
    return b + 1;
}

/* read the whole file into one scratch buffer so the zip walk and part
   extraction run over memory instead of 64KB fread chunks */
static char *slurp_file(const char *path, size_t *out_n)
{
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;
    if (fseeko(f, 0, SEEK_END) != 0) { fclose(f); return NULL; }
    off_t sz = ftello(f);
    if (sz < 0 || fseeko(f, 0, SEEK_SET) != 0) { fclose(f); return NULL; }
    char *buf = scratch_alloc((size_t)sz + 1);
    if (sz > 0 && fread(buf, 1, (size_t)sz, f) != (size_t)sz) {
        fclose(f);
        return NULL;
    }
    fclose(f);
    *out_n = (size_t)sz;
    return buf;
}

/* Every zip member records a CRC-32 of its uncompressed bytes.  Extraction
   here bypasses miniz's own reader, so the check is done here instead:
   without it a flipped byte in a stored part reaches R as a plausible cell
   value.  Slice-by-eight so verifying costs a fraction of inflating; the
   table is built once from R_init_rcxl, before any worker thread exists. */
static uint32_t crc_tab[8][256];

void rcxl_crc32_init(void)
{
    for (unsigned n = 0; n < 256; n++) {
        uint32_t c = n;
        for (int k = 0; k < 8; k++)
            c = (c & 1) ? 0xEDB88320u ^ (c >> 1) : c >> 1;
        crc_tab[0][n] = c;
    }
    for (unsigned n = 0; n < 256; n++)
        for (int k = 1; k < 8; k++)
            crc_tab[k][n] = (crc_tab[k - 1][n] >> 8) ^
                            crc_tab[0][crc_tab[k - 1][n] & 0xFF];
}

static uint32_t crc32_of(const void *buf, size_t n)
{
    const unsigned char *p = (const unsigned char *)buf;
    uint32_t c = 0xFFFFFFFFu;
    while (n >= 8) {
        c ^= (uint32_t)p[0] | ((uint32_t)p[1] << 8) |
             ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
        p += 8;
        n -= 8;
        c = crc_tab[7][c & 0xFF] ^ crc_tab[6][(c >> 8) & 0xFF] ^
            crc_tab[5][(c >> 16) & 0xFF] ^ crc_tab[4][c >> 24] ^
            crc_tab[3][p[-4]] ^ crc_tab[2][p[-3]] ^
            crc_tab[1][p[-2]] ^ crc_tab[0][p[-1]];
    }
    while (n--) c = crc_tab[0][(c ^ *p++) & 0xFF] ^ (c >> 8);
    return c ^ 0xFFFFFFFFu;
}

/* zip container: miniz walks the central directory; payload bytes are
   located via the local header and inflated with libdeflate */
typedef struct {
    mz_zip_archive za;
    const char *data;
    size_t n;
    struct libdeflate_decompressor *infl;
} zipsrc_t;

/* libdeflate allocations come from the scratch arena so an error() longjmp
   cannot leak them; free is a no-op for the same reason */
static void *ld_malloc(size_t n) { return scratch_alloc(n); }
static void ld_free(void *p) { (void)p; }

/* zsrc_ prefix: the macOS 11 SDK declares a BSD zopen() in <stdio.h>, and a
   static function of the same name fails to compile there */
static int zsrc_open(zipsrc_t *z, const char *path)
{
    z->data = slurp_file(path, &z->n);
    if (!z->data) return 0;
    memset(&z->za, 0, sizeof z->za);
    if (!mz_zip_reader_init_mem(&z->za, z->data, z->n, 0)) return 0;
    struct libdeflate_options opt = {sizeof opt, ld_malloc, ld_free};
    z->infl = libdeflate_alloc_decompressor_ex(&opt);
    if (!z->infl) { mz_zip_reader_end(&z->za); return 0; }
    return 1;
}

static void zsrc_close(zipsrc_t *z)
{
    mz_zip_reader_end(&z->za);
}

/* locate the part's payload in the whole-file buffer: the local header's own
   name/extra lengths can differ from the central directory's copy */
static const char *zsrc_payload(const zipsrc_t *z, const mz_zip_archive_file_stat *st)
{
    mz_uint64 ofs = st->m_local_header_ofs;
    if (ofs + 30 > z->n) return NULL;
    const unsigned char *lh = (const unsigned char *)z->data + ofs;
    if (memcmp(lh, "PK\x03\x04", 4) != 0) return NULL;
    size_t namelen = (size_t)lh[26] | ((size_t)lh[27] << 8);
    size_t extralen = (size_t)lh[28] | ((size_t)lh[29] << 8);
    mz_uint64 start = ofs + 30 + namelen + extralen;
    if (start + st->m_comp_size > z->n) return NULL;
    return z->data + start;
}

/* A part that is absent and one that is present but unreadable need
   different handling: several parts are optional, and silently treating a
   corrupt one as missing would drop shared strings or date styles. */
enum { ZPART_CORRUPT = -1, ZPART_MISSING = 0, ZPART_OK = 1 };

static int zsrc_read(zipsrc_t *z, const char *name, buf_t *out)
{
    int idx = mz_zip_reader_locate_file(&z->za, name, NULL, 0);
    if (idx < 0) return ZPART_MISSING;
    mz_zip_archive_file_stat st;
    if (!mz_zip_reader_file_stat(&z->za, (mz_uint)idx, &st)) return ZPART_CORRUPT;
    const char *payload = zsrc_payload(z, &st);
    if (!payload) return ZPART_CORRUPT;
    char *buf = scratch_alloc((size_t)st.m_uncomp_size + 1);
    if (st.m_method == 0) {
        if (st.m_comp_size < st.m_uncomp_size) return ZPART_CORRUPT;
        memcpy(buf, payload, (size_t)st.m_uncomp_size);
    } else if (st.m_method == 8) {
        size_t got = 0;
        if (libdeflate_deflate_decompress(z->infl, payload, (size_t)st.m_comp_size,
                                          buf, (size_t)st.m_uncomp_size, &got)
                != LIBDEFLATE_SUCCESS || got != (size_t)st.m_uncomp_size)
            return ZPART_CORRUPT;
    } else
        return ZPART_CORRUPT;
    if (crc32_of(buf, (size_t)st.m_uncomp_size) != st.m_crc32)
        return ZPART_CORRUPT;
    buf[st.m_uncomp_size] = 0;
    out->p = buf;
    out->n = (size_t)st.m_uncomp_size;
    return ZPART_OK;
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


static str_t str_trim(str_t s);

/* User-supplied NA strings.  Entries are matched byte-exact against
   whitespace-trimmed cell text; entries that themselves parse as numbers
   additionally blank numeric cells by value, so "-999" catches the sentinel
   however Excel chose to store it.  Built once per read on the main thread,
   read-only afterwards (workers may match concurrently). */
typedef struct {
    str_t *v;        /* non-empty entries, UTF-8 */
    double *num;     /* the entries that parse as numbers */
    int n, nnum;
    int has_empty;   /* "" in the set: empty/whitespace-only text reads NA */
    int min_len, max_len;
} naset_t;

/* s must already be whitespace-trimmed */
static int na_match(const naset_t *na, str_t s)
{
    if (s.n == 0) return na->has_empty;
    if (s.n < na->min_len || s.n > na->max_len) return 0;
    for (int i = 0; i < na->n; i++)
        if (na->v[i].n == s.n && memcmp(na->v[i].p, s.p, (size_t)s.n) == 0)
            return 1;
    return 0;
}

static int na_match_num(const naset_t *na, double d)
{
    for (int i = 0; i < na->nnum; i++)
        if (na->num[i] == d) return 1;
    return 0;
}

static void parse_sst(buf_t sst, str_t **out, long *n_out, unsigned char **na_out,
                      const naset_t *naset)
{
    *out = NULL;
    *n_out = 0;
    *na_out = NULL;
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
    /* uniqueCount only presizes the table, which doubles as needed, so an
       inflated count must not turn into an inflated allocation */
    if (cap <= 0 || cap > (1L << 20)) cap = 1024;
    str_t *arr = (str_t *)scratch_alloc((size_t)cap * sizeof(str_t));
    unsigned char *blank = (unsigned char *)scratch_alloc((size_t)cap);
    long n = 0;
    const char *p = sst.p;
    while ((p = find_tag(p, end, "<si")) != NULL) {
        int self;
        const char *body = tag_close(p + 3, end, &self);
        const char *stop = self ? body : sfind(body, end, "</si>");
        if (!stop) stop = end;
        if (n == cap) {
            str_t *na = (str_t *)scratch_alloc((size_t)cap * 2 * sizeof(str_t));
            unsigned char *nb = (unsigned char *)scratch_alloc((size_t)cap * 2);
            memcpy(na, arr, (size_t)n * sizeof(str_t));
            memcpy(nb, blank, (size_t)n);
            arr = na;
            blank = nb;
            cap *= 2;
        }
        if (self) {
            arr[n].p = NULL;
            arr[n].n = 0;
        } else
            arr[n] = collect_text(body, stop);
        /* cells referencing na-matched entries (empty/whitespace-only by
           default) read as NA; matched once per unique string, so every
           later cell hit is a single byte lookup */
        blank[n] = (unsigned char)na_match(naset, str_trim(arr[n]));
        n++;
        p = stop;
    }
    *out = arr;
    *n_out = n;
    *na_out = blank;
}


/* one of want_name / want_idx (1-based workbook position) selects the sheet */
static long wb_sheet_lookup(buf_t wb, const char *want_name, long want_idx,
                            char *rid, size_t rid_cap, str_t *name_out)
{
    const char *end = wb.p + wb.n;
    const char *p = wb.p;
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

static long wb_sheet_count(buf_t wb)
{
    const char *end = wb.p + wb.n;
    long n = 0;
    const char *p = wb.p;
    while ((p = find_tag(p, end, "<sheet")) != NULL) { n++; p += 6; }
    return n;
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


/* workbook-level state shared by every sheet read in one call: the zip
   source plus the parsed workbook, relationships, styles, and SST */
typedef struct {
    zipsrc_t za;
    buf_t wb, rels;
    unsigned char *xf_date;
    int n_xf;
    str_t *sst;
    unsigned char *sst_na;
    long n_sst;
    int date1904;
    naset_t na;
    const char *bad_part;  /* part that failed to extract in wb_meta; the
                              caller reports it once the zip is closed */
} wb_t;

static void wb_open(wb_t *w, const char *cpath)
{
    memset(w, 0, sizeof *w);
    scratch_reset();
    if (!zsrc_open(&w->za, cpath))
        error("cannot open '%s' as a zip archive", cpath);
    int rc = zsrc_read(&w->za, "xl/workbook.xml", &w->wb);
    if (rc != ZPART_OK) {
        zsrc_close(&w->za);
        if (rc == ZPART_CORRUPT)
            error("'%s' is corrupt: cannot extract xl/workbook.xml", cpath);
        error("'%s' has no xl/workbook.xml; not an xlsx file", cpath);
    }
    if (zsrc_read(&w->za, "xl/_rels/workbook.xml.rels", &w->rels) == ZPART_CORRUPT) {
        zsrc_close(&w->za);
        error("'%s' is corrupt: cannot extract xl/_rels/workbook.xml.rels",
              cpath);
    }
}

/* separate from wb_open so the first worksheet's inflation can overlap it */
static void wb_meta(wb_t *w)
{
    buf_t sstbuf = {NULL, 0}, sty = {NULL, 0};
    if (zsrc_read(&w->za, "xl/sharedStrings.xml", &sstbuf) == ZPART_CORRUPT)
        w->bad_part = "xl/sharedStrings.xml";
    else if (zsrc_read(&w->za, "xl/styles.xml", &sty) == ZPART_CORRUPT)
        w->bad_part = "xl/styles.xml";
    w->date1904 = wb_date1904(w->wb);
    parse_styles(sty, &w->xf_date, &w->n_xf);
    parse_sst(sstbuf, &w->sst, &w->n_sst, &w->sst_na, &w->na);
}


enum {
    CELL_BLANK = 0,
    CELL_NUM,
    CELL_DATE,
    CELL_SST,
    CELL_STR,
    CELL_BOOL,
    CELL_EMPTY,  /* had a value matching the na set (empty/whitespace-only
                    text by default): counts for sheet extent, but is NA and
                    never affects typing */
    CELL_STR_RAW,/* value slice still XML-escaped; decoded at materialization */
    CELL_IS_RAW  /* <is> body slice; runs collected at materialization */
};

/* per-column type overrides; order matches the R-side name vector */
enum {
    COL_GUESS = 0,
    COL_SKIP,
    COL_LGL,
    COL_NUM,
    COL_DATE,
    COL_TEXT,
    COL_LIST
};

typedef struct {
    unsigned char *tag;
    double *num;
    str_t *str;      /* lazily allocated; only for CELL_STR */
    R_xlen_t cap;
    /* running tallies maintained as cells land, so typing and extent need
       no extra passes over the grid afterward */
    R_xlen_t nnum, ndate, nbool, nstr;
    R_xlen_t nonblank;   /* includes CELL_EMPTY: counts toward extent */
} col_t;

typedef struct {
    col_t *cols;
    int ncols, ncols_cap;
    R_xlen_t nrow;
    R_xlen_t row_cap_hint;
    R_xlen_t rmin, rmax; /* global non-blank row extent; rmax < 0 = none */
} grid_t;


static void grid_ensure_col(grid_t *g, int col)
{
    if (col < g->ncols) return;
    if (col >= g->ncols_cap) {
        int nc = g->ncols_cap ? g->ncols_cap : 16;
        while (nc <= col) nc *= 2;
        col_t *na = (col_t *)scratch_alloc((size_t)nc * sizeof(col_t));
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
    unsigned char *tag = (unsigned char *)scratch_alloc((size_t)nc);
    double *num = (double *)scratch_alloc((size_t)nc * sizeof(double));
    memset(tag, 0, (size_t)nc);
    if (c->tag) {
        memcpy(tag, c->tag, (size_t)c->cap);
        memcpy(num, c->num, (size_t)c->cap * sizeof(double));
    }
    if (c->str) {
        str_t *str = (str_t *)scratch_alloc((size_t)nc * sizeof(str_t));
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


static unsigned char cell_tag(const col_t *c, R_xlen_t row)
{
    return row < c->cap ? c->tag[row] : (unsigned char)CELL_BLANK;
}

/* Correctly rounded decimal-to-double fast path. When the significand fits
   exactly in a double (<= 2^53) and the power of ten is exactly
   representable (|exp| <= 22), one IEEE multiply or divide of two exact
   operands is correctly rounded by construction, so the result is
   bit-identical to strtod at a fraction of its cost. Anything else -- too
   many digits, extreme exponents, inf/nan/hex forms -- returns 0 for the
   caller to hand to strtod. */
static int parse_num_fast(const char *s, int n, double *out)
{
    static const double p10[23] = {
        1e0,  1e1,  1e2,  1e3,  1e4,  1e5,  1e6,  1e7,
        1e8,  1e9,  1e10, 1e11, 1e12, 1e13, 1e14, 1e15,
        1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22
    };
    const char *p = s, *e = s + n;
    int neg = 0;
    if (p < e && (*p == '-' || *p == '+')) neg = (*p == '-'), p++;
    uint64_t sig = 0;
    int exp10 = 0, ndig = 0, saw_digit = 0, saw_dot = 0;
    for (; p < e; p++) {
        if (*p >= '0' && *p <= '9') {
            saw_digit = 1;
            if (ndig < 19) {
                sig = sig * 10 + (uint64_t)(*p - '0');
                if (sig) ndig++;
                if (saw_dot) exp10--;
            } else
                return 0;
        } else if (*p == '.') {
            if (saw_dot) return 0;
            saw_dot = 1;
        } else if (*p == 'e' || *p == 'E') {
            if (!saw_digit || ++p >= e) return 0;
            int eneg = 0, ev = 0;
            if (*p == '-' || *p == '+') { eneg = (*p == '-'); p++; }
            if (p >= e) return 0;
            for (; p < e; p++) {
                if (*p < '0' || *p > '9' || ev > 9999) return 0;
                ev = ev * 10 + (*p - '0');
            }
            exp10 += eneg ? -ev : ev;
            break;
        } else
            return 0;
    }
    if (!saw_digit || sig > (1ULL << 53)) return 0;
    double d;
    if (exp10 >= 0 && exp10 <= 22)
        d = (double)sig * p10[exp10];
    else if (exp10 < 0 && exp10 >= -22)
        d = (double)sig / p10[-exp10];
    else
        return 0;
    *out = neg ? -d : d;
    return 1;
}

/* Cell window from the R-level range/skip/n_max arguments.  Cells outside
   [r0,r1) x [c0,c1) are filtered at parse time (negative r1/c1 leaves that
   side open) and the grid is indexed relative to (r0,c0).  A fixed axis
   materializes the window's exact span instead of the trimmed data extent,
   so a range keeps its blank rows and columns. */
typedef struct {
    long r0, r1;
    int c0, c1;
    int fixed_r, fixed_c;
    R_xlen_t n_max;   /* cap on data rows when rows are not fixed; < 0 = none */
} win_t;


/* ---- sheet parsing ----
   The row loop is chunk-based so it can run serially over the whole buffer
   or data-parallel over disjoint row ranges.  Unless ck->can_grow (serial
   mode) parse_rows never calls into R: tallies go to chunk-private arrays,
   string cells store zero-copy slices, and anything that would need
   allocation (entity decoding, multi-run inline strings) is deferred to
   materialization on the main thread. */

#define CNT_NUM 0
#define CNT_DATE 1
#define CNT_BOOL 2
#define CNT_STR 3
#define CNT_NONBLANK 4
#define MAX_COLS 16384
#define MAX_ROWS 1048576

typedef struct {
    const char *begin, *end;
    grid_t *g;
    const unsigned char *xf_date;
    int n_xf;
    const unsigned char *sst_na;
    long n_sst;
    const naset_t *na;
    unsigned char *sst_used;   /* chunk-private in threaded mode */
    R_xlen_t *counts;          /* chunk-private, [MAX_COLS * 5] */
    R_xlen_t rmin, rmax;
    long row_lo, row_hi;       /* allowed 0-based rows [lo,hi); hi<0: open */
    long flt_lo, flt_hi;       /* window rows [lo,hi); hi<0: open */
    int flt_c0, flt_c1;        /* window cols [c0,c1); c1<0: open */
    int can_grow;              /* serial mode: may allocate through R */
    int fail;                  /* threaded bail: rerun serially */
    long bad_row;              /* row number past MAX_ROWS, 0 if none */
} chunk_t;

static void chunk_extent(chunk_t *ck, int col, R_xlen_t row)
{
    ck->counts[col * 5 + CNT_NONBLANK]++;
    if (row < ck->rmin) ck->rmin = row;
    if (row > ck->rmax) ck->rmax = row;
}

/* store a string cell: CELL_STR holds a ready slice; the RAW tags hold
   slices decoded at materialization time.  Text matching the na set
   (empty/whitespace-only by default) reads as NA without influencing typing
   (readxl semantics), but still counts toward the sheet extent.  RAW slices
   cannot be matched here -- they are na-checked after decoding at
   materialization instead. */
static void chunk_set_str(chunk_t *ck, col_t *c, int col, R_xlen_t row,
                          str_t s, unsigned char tag)
{
    chunk_extent(ck, col, row);
    if (tag == CELL_STR && na_match(ck->na, str_trim(s))) {
        c->tag[row] = CELL_EMPTY;
        return;
    }
    if (!c->str) {
        if (!ck->can_grow) { ck->fail = 1; return; }
        c->str = (str_t *)scratch_alloc((size_t)c->cap * sizeof(str_t));
    }
    c->tag[row] = tag;
    c->str[row] = s;
    ck->counts[col * 5 + CNT_STR]++;
}

/* a still-escaped value slice: zero-copy unless it contains entities */
static void chunk_set_value_str(chunk_t *ck, col_t *c, int col, R_xlen_t row,
                                const char *vs, int vlen)
{
    str_t s = {vs, vlen};
    if (vlen > 0 && memchr(vs, '&', (size_t)vlen) != NULL)
        chunk_set_str(ck, c, col, row, s, CELL_STR_RAW);
    else
        chunk_set_str(ck, c, col, row, s, CELL_STR);
}

/* inline string cell: zero-copy slice if it is a single entity-free run */
static int inline_zero_copy(const char *body, const char *cend, str_t *out)
{
    str_t run;
    const char *after = next_text_run(body, cend, &run);
    if (!after) { out->p = body; out->n = 0; return 1; }   /* no text runs */
    str_t run2;
    if (next_text_run(after, cend, &run2) != NULL) return 0;
    if (run.n > 0 && memchr(run.p, '&', (size_t)run.n) != NULL) return 0;
    *out = run;
    return 1;
}

static void parse_rows(chunk_t *ck)
{
    grid_t *g = ck->g;
    const char *end = ck->end;
    const char *p = ck->begin;
    long cur_row = ck->row_lo - 1;

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
        /* a row number past the sheet limit is malformed; left alone it
           sizes every column array from the bogus extent */
        if (rnum > MAX_ROWS) {
            if (!ck->can_grow) { ck->fail = 1; return; }
            ck->bad_row = rnum;
            return;
        }
        if (rnum > 0)
            cur_row = rnum - 1;
        else {
            /* implied row number: fine serially, breaks chunk disjointness */
            if (!ck->can_grow) { ck->fail = 1; return; }
            cur_row++;
        }
        if (cur_row < ck->row_lo || (ck->row_hi >= 0 && cur_row >= ck->row_hi)) {
            ck->fail = 1;
            return;
        }
        int rskip = cur_row < ck->flt_lo ||
                    (ck->flt_hi >= 0 && cur_row >= ck->flt_hi);
        if (rskip && !ck->can_grow) {
            /* threaded mode already assumes ascending rows, so past the
               window means done; hopping rows by tag is safe because a
               false "<row" match has no valid r= and fails into the
               serial path, which walks every cell instead */
            if (ck->flt_hi >= 0 && cur_row >= ck->flt_hi) break;
            continue;
        }
        R_xlen_t row = (R_xlen_t)(cur_row - ck->flt_lo);
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
            /* filtered cells are still walked to </c> so the scan stays
               aligned; only the stores are suppressed */
            int crel = col - ck->flt_c0;
            int cskip = rskip || crel < 0 || crel >= MAX_COLS ||
                        (ck->flt_c1 >= 0 && col >= ck->flt_c1);
            col_t *c = NULL;
            if (!cskip) {
                if (crel >= g->ncols) {
                    if (!ck->can_grow) { ck->fail = 1; return; }
                    grid_ensure_col(g, crel);
                }
                c = &g->cols[crel];
                if (row >= c->cap) {
                    if (!ck->can_grow) { ck->fail = 1; return; }
                    col_ensure_row(c, row, g->row_cap_hint);
                }
            }
            if (self) continue;

            if (t == 'i') {
                const char *cend = sfind(p, end, "</c>");
                if (!cend) cend = end;
                const char *is = find_tag(p, cend, "<is");
                if (is && !cskip) {
                    int s2;
                    const char *body = tag_close(is + 3, cend, &s2);
                    if (!s2) {
                        str_t s;
                        if (inline_zero_copy(body, cend, &s))
                            chunk_set_str(ck, c, crel, row, s, CELL_STR);
                        else {
                            str_t raw = {body, (int)(cend - body)};
                            chunk_set_str(ck, c, crel, row, raw, CELL_IS_RAW);
                        }
                    }
                }
                p = cend < end ? cend + 4 : end;
                continue;
            }
            if (t == 'e') continue;   /* outer loop skims the error body */

            /* one forward sweep: find <v> (skipping <f>...</f>) or hit </c> */
            const char *vs = NULL;
            const char *q3 = p;
            for (;;) {
                q3 = memchr(q3, '<', (size_t)(end - q3));
                if (!q3 || end - q3 < 4) { p = end; break; }
                if (q3[1] == 'v' && (q3[2] == '>' || q3[2] == ' ')) {
                    int s2;
                    vs = tag_close(q3 + 2, end, &s2);
                    if (s2) vs = NULL;
                    break;
                }
                if (q3[1] == '/') {
                    if (q3[2] == 'c' && q3[3] == '>') { p = q3 + 4; break; }
                    if (q3[2] == 'r') { p = q3; break; }  /* row closed early */
                    q3 += 2;
                    continue;
                }
                if (q3[1] == 'f') {   /* formula body may contain quoted "<c" */
                    const char *fe = sfind(q3, end, "</f>");
                    if (!fe) { p = end; break; }
                    q3 = fe + 4;
                    continue;
                }
                q3++;
            }
            if (!vs) continue;
            /* value text is XML-escaped, so the next '<' is exactly </v> */
            const char *ve = memchr(vs, '<', (size_t)(end - vs));
            if (!ve) ve = end;
            int vlen = (int)(ve - vs);
            p = end - ve >= 4 ? ve + 4 : end;
            if (end - p >= 4 && p[0] == '<' && p[1] == '/' && p[2] == 'c' &&
                p[3] == '>')
                p += 4;   /* skip </c> here rather than re-scanning it */
            if (cskip) continue;

            if (t == 's') {
                long i = slice_long(vs, vlen);
                chunk_extent(ck, crel, row);
                if (i < 0 || i >= ck->n_sst || ck->sst_na[i]) {
                    c->tag[row] = CELL_EMPTY;
                } else {
                    c->tag[row] = CELL_SST;
                    c->num[row] = (double)i;
                    ck->counts[crel * 5 + CNT_STR]++;
                    ck->sst_used[i] = 1;
                }
            } else if (t == 'b') {
                chunk_extent(ck, crel, row);
                c->tag[row] = CELL_BOOL;
                c->num[row] = (vlen > 0 && (vs[0] == '1' || vs[0] == 't'))
                                  ? 1.0 : 0.0;
                ck->counts[crel * 5 + CNT_BOOL]++;
            } else if (t == 'r' || t == 'd') {
                chunk_set_value_str(ck, c, crel, row, vs, vlen);
            } else if (vlen > 0 && vlen < 64) {
                double d;
                int ok = parse_num_fast(vs, vlen, &d);
                if (!ok) {
                    /* strtod, not R_strtod: correctly rounded,
                       matching the fast path bit-for-bit */
                    char nbuf[64];
                    memcpy(nbuf, vs, (size_t)vlen);
                    nbuf[vlen] = 0;
                    char *ep = NULL;
                    d = strtod(nbuf, &ep);
                    ok = ep && *ep == 0;
                }
                if (ok) {
                    chunk_extent(ck, crel, row);
                    if (ck->na->nnum && na_match_num(ck->na, d)) {
                        c->tag[row] = CELL_EMPTY;
                    } else {
                        int isdate = style >= 0 && style < ck->n_xf &&
                                     ck->xf_date[style];
                        if (isdate) {
                            c->tag[row] = CELL_DATE;
                            ck->counts[crel * 5 + CNT_DATE]++;
                        } else {
                            c->tag[row] = CELL_NUM;
                            ck->counts[crel * 5 + CNT_NUM]++;
                        }
                        c->num[row] = d;
                    }
                } else
                    chunk_set_value_str(ck, c, crel, row, vs, vlen);
            } else if (vlen > 0)
                chunk_set_value_str(ck, c, crel, row, vs, vlen);
        }
        if (ck->fail) return;
    }
}

/* <dimension>: 1-based row count and 0-based max column, or -1 if absent */
static void read_dimension(buf_t sheet, long *rows_out, int *cols_out)
{
    *rows_out = -1;
    *cols_out = -1;
    const char *end = sheet.p + sheet.n;
    const char *p = find_tag(sheet.p, end, "<dimension");
    if (!p) return;
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
                *rows_out = slice_long(r2 + i, len - i);
                *cols_out = ref_col(r2, len);
                /* the declared extent presizes the grid, so a lying or
                   hostile <dimension> must not size it past the limits */
                if (*rows_out > MAX_ROWS) *rows_out = MAX_ROWS;
                if (*cols_out >= MAX_COLS) *cols_out = MAX_COLS - 1;
            }
        }
        q = q2;
    }
}

static R_xlen_t *chunk_counts_alloc(void)
{
    R_xlen_t *counts =
        (R_xlen_t *)scratch_alloc((size_t)MAX_COLS * 5 * sizeof(R_xlen_t));
    memset(counts, 0, MAX_COLS * 5 * sizeof(R_xlen_t));
    return counts;
}

static void chunk_init(chunk_t *ck, buf_t sheet, grid_t *g, const win_t *win,
                       const unsigned char *xf_date, int n_xf,
                       const unsigned char *sst_na, long n_sst,
                       const naset_t *naset)
{
    memset(ck, 0, sizeof *ck);
    ck->begin = sheet.p;
    ck->end = sheet.p + sheet.n;
    ck->g = g;
    ck->xf_date = xf_date;
    ck->n_xf = n_xf;
    ck->sst_na = sst_na;
    ck->n_sst = n_sst;
    ck->na = naset;
    ck->counts = chunk_counts_alloc();
    ck->rmin = R_XLEN_T_MAX;
    ck->rmax = -1;
    ck->row_hi = -1;
    ck->flt_lo = win->r0;
    ck->flt_hi = win->r1;
    ck->flt_c0 = win->c0;
    ck->flt_c1 = win->c1;
}

static void merge_chunk(grid_t *g, const chunk_t *ck,
                        unsigned char *sst_used, long n_sst)
{
    for (int j = 0; j < g->ncols; j++) {
        col_t *c = &g->cols[j];
        const R_xlen_t *ct = ck->counts + (size_t)j * 5;
        c->nnum += ct[CNT_NUM];
        c->ndate += ct[CNT_DATE];
        c->nbool += ct[CNT_BOOL];
        c->nstr += ct[CNT_STR];
        c->nonblank += ct[CNT_NONBLANK];
    }
    if (ck->rmin < g->rmin) g->rmin = ck->rmin;
    if (ck->rmax > g->rmax) g->rmax = ck->rmax;
    if (sst_used && ck->sst_used && ck->sst_used != sst_used)
        for (long i = 0; i < n_sst; i++)
            sst_used[i] |= ck->sst_used[i];
}

/* window intersected with the sheet dimension, in window-relative units:
   number of rows the grid can receive and the highest relative column */
static void win_dims(const win_t *win, long dim_rows, int dim_cols,
                     long *rows_out, int *cmax_out)
{
    long hi = win->r1 >= 0 && win->r1 < dim_rows ? win->r1 : dim_rows;
    *rows_out = hi - win->r0;
    int cmax = win->c1 >= 0 && win->c1 - 1 < dim_cols ? win->c1 - 1 : dim_cols;
    *cmax_out = cmax - win->c0;
}

static void parse_sheet_serial(buf_t sheet, grid_t *g, const win_t *win,
                               const unsigned char *xf_date, int n_xf,
                               const unsigned char *sst_na, long n_sst,
                               unsigned char *sst_used, const naset_t *naset)
{
    long dim_rows;
    int dim_cols;
    read_dimension(sheet, &dim_rows, &dim_cols);
    long win_rows;
    int crel_max;
    win_dims(win, dim_rows, dim_cols, &win_rows, &crel_max);
    if (dim_rows > 0 && win_rows > 0) g->row_cap_hint = win_rows;
    if (dim_cols >= 0 && crel_max >= 0 && crel_max < MAX_COLS)
        grid_ensure_col(g, crel_max);

    chunk_t ck;
    chunk_init(&ck, sheet, g, win, xf_date, n_xf, sst_na, n_sst, naset);
    ck.sst_used = sst_used;   /* serial: write the shared map directly */
    ck.can_grow = 1;
    parse_rows(&ck);
    if (ck.bad_row)
        error("row number %ld exceeds the xlsx limit of %d", ck.bad_row,
              MAX_ROWS);
    merge_chunk(g, &ck, NULL, 0);
}

static void *parse_rows_thread(void *arg)
{
    parse_rows((chunk_t *)arg);
    return NULL;
}

/* Data-parallel parse: split at <row boundaries; every boundary row's r=
   pins the disjoint 0-based row range each worker may write.  Workers touch
   no R API.  Returns 1 on success, 0 to rerun serially (missing dimension
   or r=, out-of-range rows, thread failure). */
static int parse_sheet_mt(buf_t sheet, grid_t *g, const win_t *win,
                          const unsigned char *xf_date, int n_xf,
                          const unsigned char *sst_na, long n_sst,
                          unsigned char *sst_used, const naset_t *naset,
                          int nthreads)
{
    const char *end = sheet.p + sheet.n;
    long dim_rows;
    int dim_cols;
    read_dimension(sheet, &dim_rows, &dim_cols);
    if (dim_rows <= 0 || dim_cols < 0 || dim_cols >= MAX_COLS) return 0;
    long win_rows;
    int crel_max;
    win_dims(win, dim_rows, dim_cols, &win_rows, &crel_max);
    /* window past the dimension: dimensions lie, let the serial pass look */
    if (win_rows <= 0) return 0;

    const char *first = find_tag(sheet.p, end, "<row");
    if (!first) {          /* no rows at all: nothing to parse */
        g->row_cap_hint = win_rows;
        return 1;
    }

    /* pre-size the whole grid: workers cannot allocate */
    g->row_cap_hint = win_rows;
    if (crel_max >= 0) grid_ensure_col(g, crel_max);
    for (int j = 0; j < g->ncols; j++) {
        col_t *c = &g->cols[j];
        col_ensure_row(c, (R_xlen_t)win_rows - 1, win_rows);
        if (!c->str)
            c->str = (str_t *)scratch_alloc((size_t)c->cap * sizeof(str_t));
    }

    const char **bounds =
        (const char **)scratch_alloc(((size_t)nthreads + 1) * sizeof(char *));
    long *firstrow = (long *)scratch_alloc((size_t)nthreads * sizeof(long));
    int used = 1;
    bounds[0] = first;
    for (int i = 1; i < nthreads; i++) {
        const char *probe = sheet.p + ((size_t)sheet.n * (size_t)i) / (size_t)nthreads;
        if (probe <= bounds[used - 1]) continue;
        const char *b = find_tag(probe, end, "<row");
        if (b && b > bounds[used - 1]) bounds[used++] = b;
    }
    bounds[used] = end;

    for (int i = 0; i < used; i++) {
        long rnum = -1;
        attr_t a;
        const char *q = bounds[i] + 4;
        const char *q2;
        while ((q2 = next_attr(q, bounds[i + 1], &a)) != NULL) {
            if (attr_is(&a, "r")) rnum = slice_long(a.v, a.vlen);
            q = q2;
        }
        if (rnum <= 0 || rnum > dim_rows) return 0;
        firstrow[i] = rnum - 1;
        if (i > 0 && firstrow[i] <= firstrow[i - 1]) return 0;
    }

    chunk_t *cks = (chunk_t *)scratch_alloc((size_t)used * sizeof(chunk_t));
    for (int i = 0; i < used; i++) {
        chunk_t *ck = &cks[i];
        buf_t part;
        part.p = (char *)bounds[i];
        part.n = (size_t)(bounds[i + 1] - bounds[i]);
        chunk_init(ck, part, g, win, xf_date, n_xf, sst_na, n_sst, naset);
        if (n_sst) {
            ck->sst_used = (unsigned char *)scratch_alloc((size_t)n_sst);
            memset(ck->sst_used, 0, (size_t)n_sst);
        }
        ck->row_lo = firstrow[i];
        ck->row_hi = i + 1 < used ? firstrow[i + 1] : dim_rows;
    }

    pthread_t *th = (pthread_t *)scratch_alloc((size_t)used * sizeof(pthread_t));
    unsigned char *live = (unsigned char *)scratch_alloc((size_t)used);
    memset(live, 0, (size_t)used);
    for (int i = 1; i < used; i++)
        live[i] = pthread_create(&th[i], NULL, parse_rows_thread, &cks[i]) == 0;
    parse_rows(&cks[0]);
    for (int i = 1; i < used; i++)
        if (!live[i]) parse_rows(&cks[i]);   /* spawn failed: run inline */
    for (int i = 1; i < used; i++)
        if (live[i]) pthread_join(th[i], NULL);

    for (int i = 0; i < used; i++)
        if (cks[i].fail) return 0;
    for (int i = 0; i < used; i++)
        merge_chunk(g, &cks[i], sst_used, n_sst);
    return 1;
}

/* worker count: RCXL_THREADS overrides; small sheets stay serial.  CRAN's
   check farm caps packages at 2 cores and signals it via
   _R_CHECK_LIMIT_CORES_, so the default honors that. */
static int rcxl_nthreads(size_t sheet_bytes)
{
    int n;
    const char *e = getenv("RCXL_THREADS");
    if (e && *e) {
        long v = strtol(e, NULL, 10);
        n = v < 1 ? 1 : (v > 64 ? 64 : (int)v);
    } else {
        if (sheet_bytes < ((size_t)4 << 20)) return 1;
        n = xthread_ncores();
        if (n > 8) n = 8;
        if (n < 1) n = 1;
    }
    /* the cap applies to an explicit RCXL_THREADS too: otherwise the setting
       sitting in someone's ~/.Renviron follows them into R CMD check */
    const char *lim = getenv("_R_CHECK_LIMIT_CORES_");
    if (lim && *lim && strcmp(lim, "false") != 0 && n > 2) n = 2;
    return n;
}


static SEXP cell_charsxp(const col_t *c, R_xlen_t row, SEXP sst_table, long n_sst,
                         int trim, const naset_t *na)
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
        return mk_utf8(s.p, s.n);
    }
    case CELL_STR_RAW:
    case CELL_IS_RAW: {
        /* deferred decode: entity-laden values and multi-run inline strings */
        str_t raw = c->str[row];
        str_t s = cell_tag(c, row) == CELL_STR_RAW
                      ? xml_unescape(raw.p, raw.n)
                      : collect_text(raw.p, raw.p + raw.n);
        str_t st = str_trim(s);
        if (na_match(na, st)) return NA_STRING;
        if (trim) s = st;
        return mk_utf8(s.p, s.n);
    }
    case CELL_NUM:
    case CELL_DATE: {
        /* integral values (the common stray-numeric-in-a-text-column case)
           skip snprintf: emit digits directly */
        double d = c->num[row];
        if (d == floor(d) && fabs(d) <= 9007199254740992.0) {
            long long v = (long long)d;
            char *e = tmp + sizeof tmp, *s = e;
            unsigned long long u = v < 0 ? 0ULL - (unsigned long long)v
                                         : (unsigned long long)v;
            do { *--s = (char)('0' + (int)(u % 10)); u /= 10; } while (u);
            if (v < 0) *--s = '-';
            return mkCharLenCE(s, (int)(e - s), CE_UTF8);
        }
        snprintf(tmp, sizeof tmp, "%.15g", d);
        return mkCharLenCE(tmp, (int)strlen(tmp), CE_UTF8);
    }
    case CELL_BOOL:
        return mkCharLenCE(c->num[row] != 0.0 ? "TRUE" : "FALSE",
                           c->num[row] != 0.0 ? 4 : 5, CE_UTF8);
    default:
        return NA_STRING;
    }
}

/* decoded text of a string-bearing cell; 0 for every other tag */
static int cell_str_slice(const wb_t *w, const col_t *c, R_xlen_t r,
                          unsigned char tg, str_t *out)
{
    switch (tg) {
    case CELL_SST: {
        long i = (long)c->num[r];
        if (i < 0 || i >= w->n_sst) return 0;
        *out = w->sst[i];
        return 1;
    }
    case CELL_STR:
        *out = c->str[r];
        return 1;
    case CELL_STR_RAW: {
        str_t raw = c->str[r];
        *out = xml_unescape(raw.p, raw.n);
        return 1;
    }
    case CELL_IS_RAW: {
        str_t raw = c->str[r];
        *out = collect_text(raw.p, raw.p + raw.n);
        return 1;
    }
    default:
        return 0;
    }
}

static int parse_num_any(str_t s, double *out)
{
    if (s.n <= 0) return 0;
    if (parse_num_fast(s.p, s.n, out)) return 1;
    char sbuf[64];
    char *nb = s.n < 64 ? sbuf : R_alloc((size_t)s.n + 1, 1);
    memcpy(nb, s.p, (size_t)s.n);
    nb[s.n] = 0;
    char *ep = NULL;
    double d = strtod(nb, &ep);
    if (!ep || ep == nb || *ep != 0) return 0;
    *out = d;
    return 1;
}

/* case-insensitive TRUE/FALSE/T/F, the values as.logical accepts */
static int parse_lgl_str(str_t s, int *out)
{
    char b[5];
    if (s.n < 1 || s.n > 5) return 0;
    for (int i = 0; i < s.n; i++) {
        char ch = s.p[i];
        b[i] = ch >= 'A' && ch <= 'Z' ? (char)(ch + 32) : ch;
    }
    if ((s.n == 4 && memcmp(b, "true", 4) == 0) || (s.n == 1 && b[0] == 't')) {
        *out = 1;
        return 1;
    }
    if ((s.n == 5 && memcmp(b, "false", 5) == 0) || (s.n == 1 && b[0] == 'f')) {
        *out = 0;
        return 1;
    }
    return 0;
}

/* 0-based column and 1-based row -> "BC12" */
static void a1_ref(int col0, long row1, char *buf, size_t cap)
{
    char tmp[8];
    int n = 0;
    long c = col0 + 1;
    while (c > 0 && n < 7) {
        tmp[n++] = (char)('A' + (int)((c - 1) % 26));
        c = (c - 1) / 26;
    }
    size_t i = 0;
    while (n > 0 && i + 1 < cap) buf[i++] = tmp[--n];
    snprintf(buf + i, cap - i, "%ld", row1);
}

/* Forced-type columns holding string cells, and every "list" column, fill
   here on the main thread: string decoding may allocate through R.  Clean
   coercions are silent; only cells lost to NA count as failures. */
static void force_col_main(const wb_t *w, SEXP col, const col_t *c, int t,
                           R_xlen_t data0, R_xlen_t r1, int date1904,
                           double epoch, int trim, SEXP sst_table, SEXP dklass,
                           SEXP tz, R_xlen_t *nfail, R_xlen_t *ffail)
{
    double *dp = t == COL_NUM || t == COL_DATE ? REAL(col) : NULL;
    int *lp = t == COL_LGL ? LOGICAL(col) : NULL;
    for (R_xlen_t r = data0; r < r1; r++) {
        /* per-cell string decoding R_allocs scratch that would otherwise
           pile up until the .Call returns; reclaim it every iteration */
        const void *vmax = vmaxget();
        R_xlen_t i = r - data0;
        unsigned char tg = cell_tag(c, r);
        if (t == COL_LIST) {
            SEXP v;
            switch (tg) {
            case CELL_NUM:
                v = ScalarReal(c->num[r]);
                break;
            case CELL_DATE: {
                v = PROTECT(ScalarReal(
                        serial_seconds(c->num[r], epoch, date1904)));
                setAttrib(v, R_ClassSymbol, dklass);
                setAttrib(v, install("tzone"), tz);
                UNPROTECT(1);
                break;
            }
            case CELL_BOOL:
                v = ScalarLogical(c->num[r] != 0.0);
                break;
            case CELL_SST:
            case CELL_STR:
            case CELL_STR_RAW:
            case CELL_IS_RAW: {
                SEXP cs = PROTECT(cell_charsxp(c, r, sst_table, w->n_sst,
                                               trim, &w->na));
                v = cs == NA_STRING ? ScalarLogical(NA_LOGICAL)
                                    : ScalarString(cs);
                UNPROTECT(1);
                break;
            }
            default:
                v = ScalarLogical(NA_LOGICAL);
                break;
            }
            SET_VECTOR_ELT(col, i, v);
            vmaxset(vmax);
            continue;
        }
        str_t s;
        int fail = 0;
        switch (t) {
        case COL_NUM:
            if (tg == CELL_NUM || tg == CELL_DATE || tg == CELL_BOOL)
                dp[i] = c->num[r];
            else if (cell_str_slice(w, c, r, tg, &s)) {
                /* na-matched RAW cells reach here undecoded; they are NA,
                   not coercion failures */
                double d;
                s = str_trim(s);
                if (na_match(&w->na, s))
                    dp[i] = NA_REAL;
                else if (parse_num_any(s, &d))
                    dp[i] = d;
                else {
                    dp[i] = NA_REAL;
                    fail = s.n > 0;
                }
            } else
                dp[i] = NA_REAL;
            break;
        case COL_LGL:
            if (tg == CELL_BOOL || tg == CELL_NUM)
                lp[i] = c->num[r] != 0.0;
            else if (tg == CELL_DATE) {
                lp[i] = NA_LOGICAL;
                fail = 1;
            } else if (cell_str_slice(w, c, r, tg, &s)) {
                int b;
                s = str_trim(s);
                if (s.n == 0 || na_match(&w->na, s))
                    lp[i] = NA_LOGICAL;
                else if (parse_lgl_str(s, &b))
                    lp[i] = b;
                else {
                    lp[i] = NA_LOGICAL;
                    fail = 1;
                }
            } else
                lp[i] = NA_LOGICAL;
            break;
        default:   /* COL_DATE; strings never parse as dates */
            /* Excel has no negative serials; such values are not dates */
            if ((tg == CELL_DATE || tg == CELL_NUM) && c->num[r] >= 0.0) {
                dp[i] = serial_seconds(c->num[r], epoch, date1904);
            } else if (tg == CELL_NUM || tg == CELL_DATE) {
                dp[i] = NA_REAL;
                fail = 1;
            } else if (tg == CELL_BOOL) {
                dp[i] = NA_REAL;
                fail = 1;
            } else if (cell_str_slice(w, c, r, tg, &s)) {
                dp[i] = NA_REAL;
                s = str_trim(s);
                fail = s.n > 0 && !na_match(&w->na, s);
            } else
                dp[i] = NA_REAL;
            break;
        }
        if (fail) {
            (*nfail)++;
            if (*ffail < 0) *ffail = r;
        }
        vmaxset(vmax);
    }
}

/* worksheet inflation job, so the sheet can decompress on a worker while
   the main thread reads and parses styles and shared strings */
typedef struct {
    struct libdeflate_decompressor *infl;
    const char *comp;
    size_t comp_n;
    char *out;
    size_t out_n;
    int method;
    uint32_t crc;   /* expected, from the central directory */
    int ok;
    int crc_bad;
} infjob_t;

static void *inflate_thread(void *arg)
{
    infjob_t *j = (infjob_t *)arg;
    if (j->method == 0) {
        if (j->comp_n >= j->out_n) {
            memcpy(j->out, j->comp, j->out_n);
            j->ok = 1;
        }
    } else {
        size_t got = 0;
        j->ok = libdeflate_deflate_decompress(j->infl, j->comp, j->comp_n,
                                              j->out, j->out_n, &got)
                    == LIBDEFLATE_SUCCESS && got == j->out_n;
    }
    if (j->ok && crc32_of(j->out, j->out_n) != j->crc) {
        j->ok = 0;
        j->crc_bad = 1;
    }
    return NULL;
}

/* Locate a worksheet part and start inflating it, on a worker only when
   allow_thread and the part is big enough to pay for the spawn.  Without a
   worker the buffer is ready on return; either way sheet_extract_finish
   must run before *out is used.  Returns 0 if the part is missing. */
static int sheet_extract_start(wb_t *w, const char *target, infjob_t *ij,
                               buf_t *out, pthread_t *th, int *spawned,
                               int allow_thread)
{
    memset(ij, 0, sizeof *ij);
    *spawned = 0;
    int sidx = mz_zip_reader_locate_file(&w->za.za, target, NULL, 0);
    mz_zip_archive_file_stat st;
    if (sidx < 0 || !mz_zip_reader_file_stat(&w->za.za, (mz_uint)sidx, &st) ||
        !(ij->comp = zsrc_payload(&w->za, &st)) ||
        (st.m_method != 0 && st.m_method != 8))
        return 0;
    out->n = (size_t)st.m_uncomp_size;
    out->p = scratch_alloc(out->n + 1);
    ij->comp_n = (size_t)st.m_comp_size;
    ij->out = out->p;
    ij->out_n = out->n;
    ij->method = (int)st.m_method;
    ij->crc = st.m_crc32;
    if (allow_thread && ij->method == 8 && ij->comp_n >= ((size_t)256 << 10) &&
        rcxl_nthreads(out->n) > 1) {
        struct libdeflate_options opt = {sizeof opt, ld_malloc, ld_free};
        ij->infl = libdeflate_alloc_decompressor_ex(&opt);
        if (ij->infl)
            *spawned = pthread_create(th, NULL, inflate_thread, ij) == 0;
    }
    if (!*spawned) {
        ij->infl = w->za.infl;
        inflate_thread(ij);
    }
    return 1;
}

static int sheet_extract_finish(infjob_t *ij, buf_t *out, pthread_t *th,
                                int spawned)
{
    if (spawned) pthread_join(*th, NULL);
    if (!ij->ok) return 0;
    out->p[out->n] = 0;
    return 1;
}

/* column materialization: non-string columns are plain array fills with no
   R API calls, so they can run on workers while the main thread interns
   string columns */
enum { FILL_REAL, FILL_DATE, FILL_LGL, FILL_NA, FILL_LGL_FORCE,
       FILL_DATE_FORCE };

typedef struct {
    const col_t *c;
    double *dp;
    int *lp;
    R_xlen_t data0, r1;
    int kind;
    int date1904;
    double epoch;
    /* coercion failures under a forced type; each job owns its slots */
    R_xlen_t *nfailp, *ffailp;
} filljob_t;

static void fill_fail(const filljob_t *j, R_xlen_t r)
{
    (*j->nfailp)++;
    if (*j->ffailp < 0) *j->ffailp = r;
}

static void fill_one(const filljob_t *j)
{
    const col_t *c = j->c;
    R_xlen_t data0 = j->data0, r1 = j->r1;
    switch (j->kind) {
    case FILL_REAL:
        for (R_xlen_t r = data0; r < r1; r++) {
            unsigned char tg = cell_tag(c, r);
            j->dp[r - data0] = (tg == CELL_NUM || tg == CELL_DATE ||
                                tg == CELL_BOOL)
                                   ? c->num[r] : NA_REAL;
        }
        break;
    case FILL_DATE:
        for (R_xlen_t r = data0; r < r1; r++) {
            if (cell_tag(c, r) != CELL_DATE) {
                j->dp[r - data0] = NA_REAL;
                continue;
            }
            j->dp[r - data0] =
                serial_seconds(c->num[r], j->epoch, j->date1904);
        }
        break;
    case FILL_LGL:
        for (R_xlen_t r = data0; r < r1; r++)
            j->lp[r - data0] = cell_tag(c, r) == CELL_BOOL
                                   ? (c->num[r] != 0.0) : NA_LOGICAL;
        break;
    case FILL_LGL_FORCE:
        for (R_xlen_t r = data0; r < r1; r++) {
            unsigned char tg = cell_tag(c, r);
            if (tg == CELL_BOOL || tg == CELL_NUM)
                j->lp[r - data0] = c->num[r] != 0.0;
            else {
                j->lp[r - data0] = NA_LOGICAL;
                if (tg == CELL_DATE) fill_fail(j, r);
            }
        }
        break;
    case FILL_DATE_FORCE:
        for (R_xlen_t r = data0; r < r1; r++) {
            unsigned char tg = cell_tag(c, r);
            /* Excel has no negative serials; such values are not dates */
            if ((tg == CELL_DATE || tg == CELL_NUM) && c->num[r] >= 0.0) {
                j->dp[r - data0] =
                    serial_seconds(c->num[r], j->epoch, j->date1904);
            } else {
                j->dp[r - data0] = NA_REAL;
                if (tg == CELL_BOOL || tg == CELL_NUM || tg == CELL_DATE)
                    fill_fail(j, r);
            }
        }
        break;
    default:
        for (R_xlen_t r = data0; r < r1; r++) j->lp[r - data0] = NA_LOGICAL;
        break;
    }
}

typedef struct {
    const filljob_t *jobs;
    int lo, hi;
} fillspan_t;

static void *fill_thread(void *arg)
{
    const fillspan_t *sp = (const fillspan_t *)arg;
    for (int i = sp->lo; i < sp->hi; i++) fill_one(&sp->jobs[i]);
    return NULL;
}

/* parse one extracted worksheet and materialize it as a named column list;
   sst_table/sst_interned accumulate interned shared strings across sheets */
static SEXP sheet_to_df(wb_t *w, buf_t sheet, const win_t *win, SEXP namesArg,
                        int trim, SEXP ctypesArg, SEXP sst_table,
                        unsigned char *sst_interned)
{
    int user_names = TYPEOF(namesArg) == STRSXP;
    int want_names = !user_names && asLogical(namesArg) == TRUE;
    const unsigned char *xf_date = w->xf_date;
    int n_xf = w->n_xf;
    const unsigned char *sst_na = w->sst_na;
    long n_sst = w->n_sst;
    int date1904 = w->date1904;
    unsigned char *sst_used =
        n_sst ? (unsigned char *)scratch_alloc((size_t)n_sst) : NULL;
    if (sst_used) memset(sst_used, 0, (size_t)n_sst);

    grid_t g;
    memset(&g, 0, sizeof g);
    g.rmin = R_XLEN_T_MAX;
    g.rmax = -1;
    int nth = rcxl_nthreads(sheet.n);
    int parsed = 0;
    if (nth > 1) {
        parsed = parse_sheet_mt(sheet, &g, win, xf_date, n_xf, sst_na,
                                n_sst, sst_used, &w->na, nth);
        if (!parsed) {   /* bail: reset and rerun serially */
            memset(&g, 0, sizeof g);
            g.rmin = R_XLEN_T_MAX;
            g.rmax = -1;
            if (sst_used) memset(sst_used, 0, (size_t)n_sst);
        }
    }
    if (!parsed)
        parse_sheet_serial(sheet, &g, win, xf_date, n_xf, sst_na, n_sst,
                           sst_used, &w->na);

    /* only shared strings some cell actually references become CHARSXPs;
       entries interned for an earlier sheet are reused as-is */
    for (long i = 0; i < n_sst; i++) {
        if (!sst_used[i] || sst_interned[i]) continue;
        str_t s = trim ? str_trim(w->sst[i]) : w->sst[i];
        SET_STRING_ELT(sst_table, i, mk_utf8(s.p, s.n));
        sst_interned[i] = 1;
    }

    /* extent excluding fully-blank edge rows/columns, from parse-time tallies */
    R_xlen_t r0 = 0, r1 = 0;
    int c0 = 0, c1 = 0;
    if (g.rmax >= 0) {
        r0 = g.rmin;
        r1 = g.rmax + 1;
        c1 = g.ncols;
        while (c0 < g.ncols && g.cols[c0].nonblank == 0) c0++;
        while (c1 > c0 && g.cols[c1 - 1].nonblank == 0) c1--;
    }
    /* a fixed axis keeps the window's exact span, blank edges included */
    if (win->fixed_r) {
        r0 = 0;
        r1 = (R_xlen_t)(win->r1 - win->r0);
    }
    if (win->fixed_c) {
        c0 = 0;
        c1 = win->c1 - win->c0;
        if (c1 > MAX_COLS) c1 = MAX_COLS;
        if (c1 > g.ncols) grid_ensure_col(&g, c1 - 1);
    }

    R_xlen_t data0 = want_names && r1 > r0 ? r0 + 1 : r0;
    R_xlen_t n = r1 - data0;
    int capped = 0;
    if (!win->fixed_r && win->n_max >= 0 && n > win->n_max) {
        n = win->n_max;
        r1 = data0 + n;
        capped = 1;
    }
    int nc = c1 - c0;
    if (nc < 0) nc = 0;
    if (n < 0) n = 0;

    if (user_names && XLENGTH(namesArg) != nc)
        error("'col_names' has length %lld but the sheet has %d column%s",
              (long long)XLENGTH(namesArg), nc, nc == 1 ? "" : "s");

    /* per-column type overrides: a scalar recycles, otherwise one entry per
       materialized column; "skip" entries count here and are dropped below */
    int *typ = nc ? (int *)scratch_alloc((size_t)nc * sizeof(int)) : NULL;
    {
        R_xlen_t nct = ctypesArg == R_NilValue ? 0 : XLENGTH(ctypesArg);
        const int *ctv = nct ? INTEGER(ctypesArg) : NULL;
        if (nct > 1 && nct != nc)
            error("'col_types' has length %lld but the sheet has %d column%s",
                  (long long)nct, nc, nc == 1 ? "" : "s");
        for (int j = 0; j < nc; j++) {
            int t = ctv ? ctv[nct == 1 ? 0 : j] : COL_GUESS;
            if (t < COL_GUESS || t > COL_LIST)
                error("invalid 'col_types' code %d", t);
            typ[j] = t;
        }
    }
    int out_nc = 0;
    for (int j = 0; j < nc; j++) out_nc += typ[j] != COL_SKIP;

    SEXP ans = PROTECT(allocVector(VECSXP, out_nc));
    SEXP nms = PROTECT(allocVector(STRSXP, out_nc));
    double epoch = date1904 ? 24107.0 : 25569.0;

    filljob_t *jobs = out_nc
        ? (filljob_t *)scratch_alloc((size_t)out_nc * sizeof(filljob_t)) : NULL;
    int *strcol =
        out_nc ? (int *)scratch_alloc((size_t)out_nc * sizeof(int)) : NULL;
    int *datecol =
        out_nc ? (int *)scratch_alloc((size_t)out_nc * sizeof(int)) : NULL;
    int *convcol =
        out_nc ? (int *)scratch_alloc((size_t)out_nc * sizeof(int)) : NULL;
    int *src =   /* output slot -> materialized column offset */
        out_nc ? (int *)scratch_alloc((size_t)out_nc * sizeof(int)) : NULL;
    R_xlen_t *nfail = out_nc
        ? (R_xlen_t *)scratch_alloc((size_t)out_nc * sizeof(R_xlen_t)) : NULL;
    R_xlen_t *ffail = out_nc
        ? (R_xlen_t *)scratch_alloc((size_t)out_nc * sizeof(R_xlen_t)) : NULL;
    int njobs = 0, nstrcols = 0, ndatecols = 0, nconvcols = 0;

    for (int j = 0, oj = 0; j < nc; j++) {
        if (typ[j] == COL_SKIP) continue;
        col_t *c = &g.cols[c0 + j];
        R_xlen_t nnum, ndate, nbool, nstr;
        if (capped) {
            /* n_max cut rows the tallies already counted; retally the kept
               span so typing reflects only the rows returned */
            nnum = ndate = nbool = nstr = 0;
            for (R_xlen_t r = data0; r < r1; r++) {
                switch (cell_tag(c, r)) {
                case CELL_NUM:  nnum++; break;
                case CELL_DATE: ndate++; break;
                case CELL_BOOL: nbool++; break;
                case CELL_SST:
                case CELL_STR:
                case CELL_STR_RAW:
                case CELL_IS_RAW: nstr++; break;
                default: break;
                }
            }
        } else {
            nnum = c->nnum;
            ndate = c->ndate;
            nbool = c->nbool;
            nstr = c->nstr;
            /* the header row is names, not data: back its cell out of the
               tallies */
            if (data0 > r0) {
                switch (cell_tag(c, r0)) {
                case CELL_NUM:  nnum--; break;
                case CELL_DATE: ndate--; break;
                case CELL_BOOL: nbool--; break;
                case CELL_SST:
                case CELL_STR:
                case CELL_STR_RAW:
                case CELL_IS_RAW: nstr--; break;
                default: break;
                }
            }
        }

        src[oj] = j;
        nfail[oj] = 0;
        ffail[oj] = -1;
        int t = typ[j];
        SEXP col;
        filljob_t *fj = NULL;
        if (t == COL_TEXT || (t == COL_GUESS && nstr > 0)) {
            col = allocVector(STRSXP, n);
            SET_VECTOR_ELT(ans, oj, col);
            strcol[nstrcols++] = oj;
        } else if (t == COL_LIST) {
            col = allocVector(VECSXP, n);
            SET_VECTOR_ELT(ans, oj, col);
            convcol[nconvcols++] = oj;
        } else if (t != COL_GUESS) {
            col = allocVector(t == COL_LGL ? LGLSXP : REALSXP, n);
            SET_VECTOR_ELT(ans, oj, col);
            if (t == COL_DATE) datecol[ndatecols++] = oj;
            if (nstr > 0)
                convcol[nconvcols++] = oj;
            else {
                fj = &jobs[njobs++];
                fj->kind = t == COL_LGL    ? FILL_LGL_FORCE
                           : t == COL_DATE ? FILL_DATE_FORCE
                                           : FILL_REAL;
                if (t == COL_LGL) fj->lp = LOGICAL(col);
                else fj->dp = REAL(col);
            }
        } else if (nbool > 0 && nnum == 0 && ndate == 0) {
            col = allocVector(LGLSXP, n);
            SET_VECTOR_ELT(ans, oj, col);
            fj = &jobs[njobs++];
            fj->kind = FILL_LGL;
            fj->lp = LOGICAL(col);
        } else if (ndate > 0 && nnum == 0) {
            col = allocVector(REALSXP, n);
            SET_VECTOR_ELT(ans, oj, col);
            fj = &jobs[njobs++];
            fj->kind = FILL_DATE;
            fj->dp = REAL(col);
            datecol[ndatecols++] = oj;
        } else if (nnum > 0 || ndate > 0 || nbool > 0) {
            col = allocVector(REALSXP, n);
            SET_VECTOR_ELT(ans, oj, col);
            fj = &jobs[njobs++];
            fj->kind = FILL_REAL;
            fj->dp = REAL(col);
        } else {
            col = allocVector(LGLSXP, n);
            SET_VECTOR_ELT(ans, oj, col);
            fj = &jobs[njobs++];
            fj->kind = FILL_NA;
            fj->lp = LOGICAL(col);
        }
        if (fj) {
            fj->c = c;
            fj->data0 = data0;
            fj->r1 = r1;
            fj->date1904 = date1904;
            fj->epoch = epoch;
            fj->nfailp = &nfail[oj];
            fj->ffailp = &ffail[oj];
        }
        oj++;
    }

    /* array fills run on workers while the main thread interns strings */
    pthread_t fth[8];
    fillspan_t spans[8];
    int nworkers = 0;
    if (nth > 1 && njobs > 0 && n > 0) {
        int want = nth - 1;
        if (want > njobs) want = njobs;
        if (want > 8) want = 8;
        for (int i = 0; i < want; i++) {
            spans[i].jobs = jobs;
            spans[i].lo = (int)((long long)njobs * i / want);
            spans[i].hi = (int)((long long)njobs * (i + 1) / want);
            if (pthread_create(&fth[nworkers], NULL, fill_thread, &spans[i]) == 0)
                nworkers++;
            else
                fill_thread(&spans[i]);
        }
    } else {
        for (int i = 0; i < njobs; i++) fill_one(&jobs[i]);
    }

    for (int si = 0; si < nstrcols; si++) {
        int oj = strcol[si];
        SEXP col = VECTOR_ELT(ans, oj);
        col_t *c = &g.cols[c0 + src[oj]];
        for (R_xlen_t r = data0; r < r1; r++) {
            /* SST is the hot case; indices were validated at parse time */
            if (cell_tag(c, r) == CELL_SST)
                SET_STRING_ELT(col, r - data0,
                               STRING_ELT(sst_table, (R_xlen_t)c->num[r]));
            else
                SET_STRING_ELT(col, r - data0,
                               cell_charsxp(c, r, sst_table, n_sst, trim,
                                            &w->na));
        }
    }

    for (int i = 0; i < nworkers; i++) pthread_join(fth[i], NULL);

    SEXP dklass = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(dklass, 0, mkChar("POSIXct"));
    SET_STRING_ELT(dklass, 1, mkChar("POSIXt"));
    SEXP tz = PROTECT(mkString("UTC"));

    /* forced columns holding strings, and list columns, convert here after
       the join: an allocation-failure longjmp mid-conversion must not leave
       workers writing into vectors the unwind made collectable */
    for (int ci = 0; ci < nconvcols; ci++) {
        int oj = convcol[ci];
        force_col_main(w, VECTOR_ELT(ans, oj), &g.cols[c0 + src[oj]],
                       typ[src[oj]], data0, r1, date1904, epoch, trim,
                       sst_table, dklass, tz, &nfail[oj], &ffail[oj]);
    }

    for (int di = 0; di < ndatecols; di++) {
        SEXP col = VECTOR_ELT(ans, datecol[di]);
        setAttrib(col, R_ClassSymbol, dklass);
        setAttrib(col, install("tzone"), tz);
    }

    for (int oj = 0; oj < out_nc; oj++) {
        int j = src[oj];
        if (user_names) {
            SET_STRING_ELT(nms, oj, STRING_ELT(namesArg, j));
        } else if (want_names && r1 > r0) {
            SEXP nm = cell_charsxp(&g.cols[c0 + j], r0, sst_table, n_sst, trim,
                                   &w->na);
            if (nm == NA_STRING) {
                char tmp[16];
                snprintf(tmp, sizeof tmp, "V%d", j + 1);
                nm = mkCharLenCE(tmp, (int)strlen(tmp), CE_UTF8);
            }
            SET_STRING_ELT(nms, oj, nm);
        } else {
            char tmp[16];
            snprintf(tmp, sizeof tmp, "V%d", j + 1);
            SET_STRING_ELT(nms, oj, mkCharLenCE(tmp, (int)strlen(tmp), CE_UTF8));
        }
    }

    for (int oj = 0; oj < out_nc; oj++) {
        if (nfail[oj] <= 0) continue;
        int t = typ[src[oj]];
        const char *tname = t == COL_LGL ? "logical"
                            : t == COL_NUM ? "numeric" : "date";
        char ref[32];
        a1_ref(win->c0 + c0 + src[oj], (long)(win->r0 + ffail[oj]) + 1, ref,
               sizeof ref);
        warning("%lld cell%s in column '%s' could not be coerced to %s and "
                "became NA (first at %s)",
                (long long)nfail[oj], nfail[oj] == 1 ? "" : "s",
                CHAR(STRING_ELT(nms, oj)), tname, ref);
    }

    setAttrib(ans, R_NamesSymbol, nms);
    UNPROTECT(4);
    return ans;
}

static void naset_build(naset_t *na, SEXP naArg)
{
    memset(na, 0, sizeof *na);
    na->min_len = INT_MAX;
    R_xlen_t len = XLENGTH(naArg);
    if (len == 0) return;
    na->v = (str_t *)scratch_alloc((size_t)len * sizeof(str_t));
    na->num = (double *)scratch_alloc((size_t)len * sizeof(double));
    for (R_xlen_t i = 0; i < len; i++) {
        SEXP el = STRING_ELT(naArg, i);
        if (el == NA_STRING) continue;
        /* translateCharUTF8's buffer is transient: copy into the scratch
           arena, where workers may read it for the whole call */
        const char *s = translateCharUTF8(el);
        size_t n = strlen(s);
        if (n == 0) { na->has_empty = 1; continue; }
        if (n > INT_MAX) continue;
        char *cp = (char *)scratch_alloc(n + 1);
        memcpy(cp, s, n + 1);
        na->v[na->n].p = cp;
        na->v[na->n].n = (int)n;
        na->n++;
        if ((int)n < na->min_len) na->min_len = (int)n;
        if ((int)n > na->max_len) na->max_len = (int)n;
        char *ep = NULL;
        double d = strtod(cp, &ep);
        if (ep && ep != cp && *ep == 0) na->num[na->nnum++] = d;
    }
}

/* Shared driver: resolve the requested sheets (R_NilValue = every sheet, in
   workbook order), extract them while workbook metadata parses, then
   materialize each.  Returns a list named by actual sheet names. */
static SEXP read_impl(const char *cpath, SEXP sheetsArg, SEXP namesArg,
                      int trim, SEXP ctypesArg, SEXP naArg, const win_t *win)
{
    /* validated before wb_open: an error() here must not skip zsrc_close */
    if (TYPEOF(naArg) != STRSXP) error("'na' must be a character vector");
    if (TYPEOF(namesArg) != STRSXP &&
        (TYPEOF(namesArg) != LGLSXP || XLENGTH(namesArg) != 1))
        error("'col_names' must be TRUE, FALSE or a character vector");
    wb_t w;
    wb_open(&w, cpath);
    naset_build(&w.na, naArg);

    R_xlen_t nsel = sheetsArg == R_NilValue ? (R_xlen_t)wb_sheet_count(w.wb)
                                            : XLENGTH(sheetsArg);
    char *targets = nsel ? (char *)scratch_alloc((size_t)nsel * 512) : NULL;
    str_t *names =
        nsel ? (str_t *)scratch_alloc((size_t)nsel * sizeof(str_t)) : NULL;
    for (R_xlen_t i = 0; i < nsel; i++) {
        const char *want_name = NULL;
        long want_idx = -1;
        if (sheetsArg == R_NilValue)
            want_idx = (long)i + 1;
        else if (TYPEOF(sheetsArg) == STRSXP)
            want_name = translateCharUTF8(STRING_ELT(sheetsArg, i));
        else
            want_idx = INTEGER(sheetsArg)[i];
        char rid[64];
        long idx = wb_sheet_lookup(w.wb, want_name, want_idx, rid, sizeof rid,
                                   &names[i]);
        if (idx < 0) {
            zsrc_close(&w.za);
            if (want_name) error("sheet '%s' not found", want_name);
            error("sheet %ld not found", want_idx);
        }
        char *target = targets + (size_t)i * 512;
        if (!rels_target(w.rels, rid, target, 512))
            snprintf(target, 512, "xl/worksheets/sheet%ld.xml", idx);
    }

    /* the first sheet inflates on a worker while metadata parses; the rest
       extract inline afterwards.  (An allocation failure in that window
       would longjmp past the join -- an accepted, OOM-only leak of the
       worker's target buffer.) */
    buf_t *sheets =
        nsel ? (buf_t *)scratch_alloc((size_t)nsel * sizeof(buf_t)) : NULL;
    infjob_t ij;
    pthread_t ith;
    int spawned = 0;
    const char *fail = NULL;
    int fail_crc = 0;
    if (nsel > 0 &&
        !sheet_extract_start(&w, targets, &ij, &sheets[0], &ith, &spawned, 1))
        fail = targets;
    wb_meta(&w);
    if (!fail && w.bad_part) {
        fail = w.bad_part;
        fail_crc = 1;
    }
    for (R_xlen_t i = 1; !fail && i < nsel; i++) {
        infjob_t ij2;
        pthread_t th2;
        int sp2;
        if (!sheet_extract_start(&w, targets + (size_t)i * 512, &ij2,
                                 &sheets[i], &th2, &sp2, 0) ||
            !sheet_extract_finish(&ij2, &sheets[i], &th2, sp2)) {
            fail = targets + (size_t)i * 512;
            fail_crc = ij2.crc_bad;
        }
    }
    if (nsel > 0 && !fail &&
        !sheet_extract_finish(&ij, &sheets[0], &ith, spawned)) {
        fail = targets;
        fail_crc = ij.crc_bad;
    } else if (spawned && fail)
        pthread_join(ith, NULL);
    zsrc_close(&w.za);
    if (fail_crc)
        error("'%s' is corrupt: CRC mismatch in part '%s'", cpath, fail);
    if (fail) error("cannot extract worksheet part '%s'", fail);

    SEXP sst_table = PROTECT(allocVector(STRSXP, w.n_sst));
    unsigned char *interned =
        w.n_sst ? (unsigned char *)scratch_alloc((size_t)w.n_sst) : NULL;
    if (interned) memset(interned, 0, (size_t)w.n_sst);

    SEXP out = PROTECT(allocVector(VECSXP, nsel));
    SEXP onms = PROTECT(allocVector(STRSXP, nsel));
    for (R_xlen_t i = 0; i < nsel; i++) {
        str_t dec = xml_unescape(names[i].p, names[i].n);
        SET_STRING_ELT(onms, i, mk_utf8(dec.p, dec.n));
    }
    for (R_xlen_t i = 0; i < nsel; i++)
        SET_VECTOR_ELT(out, i,
                       sheet_to_df(&w, sheets[i], win, namesArg, trim,
                                   ctypesArg, sst_table, interned));
    setAttrib(out, R_NamesSymbol, onms);
    UNPROTECT(3);
    return out;
}

/* {r0, r1, c0, c1, fixed_r, fixed_c, n_max} built by the R wrappers */
static win_t win_decode(SEXP w)
{
    if (TYPEOF(w) != INTSXP || XLENGTH(w) != 7) error("invalid window");
    const int *v = INTEGER(w);
    win_t win;
    win.r0 = v[0];
    win.r1 = v[1];
    win.c0 = v[2];
    win.c1 = v[3];
    win.fixed_r = v[4];
    win.fixed_c = v[5];
    win.n_max = v[6];
    return win;
}

SEXP C_read_xlsx(SEXP path, SEXP sheetArg, SEXP colNamesArg, SEXP trimArg,
                 SEXP winArg, SEXP colTypesArg, SEXP naArg)
{
    const char *cpath = translateChar(STRING_ELT(path, 0));
    if (XLENGTH(sheetArg) != 1) error("'sheet' must select a single sheet");
    win_t win = win_decode(winArg);
    SEXP out = PROTECT(read_impl(cpath, sheetArg, colNamesArg,
                                 asLogical(trimArg) == TRUE, colTypesArg,
                                 naArg, &win));
    SEXP ans = VECTOR_ELT(out, 0);
    UNPROTECT(1);
    return ans;
}

SEXP C_read_xlsx_all(SEXP path, SEXP sheetsArg, SEXP colNamesArg, SEXP trimArg,
                     SEXP winArg, SEXP colTypesArg, SEXP naArg)
{
    const char *cpath = translateChar(STRING_ELT(path, 0));
    win_t win = win_decode(winArg);
    return read_impl(cpath, sheetsArg, colNamesArg,
                     asLogical(trimArg) == TRUE, colTypesArg, naArg, &win);
}

SEXP C_sheet_names(SEXP path)
{
    const char *cpath = translateChar(STRING_ELT(path, 0));
    scratch_reset();
    zipsrc_t za;
    if (!zsrc_open(&za, cpath))
        error("cannot open '%s' as a zip archive", cpath);
    buf_t wb = {NULL, 0};
    int rc = zsrc_read(&za, "xl/workbook.xml", &wb);
    if (rc != ZPART_OK) {
        zsrc_close(&za);
        if (rc == ZPART_CORRUPT)
            error("'%s' is corrupt: cannot extract xl/workbook.xml", cpath);
        error("'%s' has no xl/workbook.xml; not an xlsx file", cpath);
    }
    zsrc_close(&za);

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
        SET_STRING_ELT(ans, i++, mk_utf8(dec.p, dec.n));
    }
    UNPROTECT(1);
    return ans;
}
