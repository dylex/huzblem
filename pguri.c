#include <string.h>
#include <stdlib.h>
#include <postgres.h>
#include <fmgr.h>
#include <funcapi.h>
#include <catalog/pg_type.h>
#include <libpq/pqformat.h>
#include <utils/builtins.h>
#include <utils/array.h>
#include <utils/typcache.h>

PG_MODULE_MAGIC;

PG_FUNCTION_INFO_V1(unsafe_cast);
Datum unsafe_cast(PG_FUNCTION_ARGS);
Datum unsafe_cast(PG_FUNCTION_ARGS)
{
	PG_RETURN_DATUM(PG_GETARG_DATUM(0));
}

#define MIN(A, B) ((B) < (A) ? (B) : (A))

#define STRSEARCH(STR, LEN, P) ({ \
		typeof(LEN) _len = LEN; \
		while (_len-- && !(P)) STR ++; \
		STR; \
	})

static const char *strndigit(const char *s, size_t len)
{
	return STRSEARCH(s, len, *s < '0' || *s > '9');
}

static void domainname_flip(char *out, const char *in, size_t len)
{
	unsigned i;
	const char *e = in+len;
	const char *p = in, *n;
	char *o = out+len;

	for (i = 0; i < 4 && p < e; i ++)
	{
		p = strndigit(p, MIN(3,e-p));
		if (p >= e || *p != '.')
			break;
		if (i < 3 && *p == '.')
			p ++;
	}

	if (p >= e)
	{
		/* looks like an IP: as is */
		memcpy(out, in, len);
		return;
	}

	n = in;
	while (1)
	{
		p = n;
		STRSEARCH(n, e-p, *n == '.');
		o -= n-p;
		memcpy(o, p, n-p);
		if (n >= e)
			break;
		*--o = *n++;
	}
}

static text *domainname_new(const char *str, size_t len)
{
	text *out;
	if (len && str[len-1] == '.')
		len --;
	out = (text *)palloc(VARHDRSZ + len);
	SET_VARSIZE(out, VARHDRSZ + len);
	domainname_flip(VARDATA(out), str, len);
	return out;
}

PG_FUNCTION_INFO_V1(domainname_in);
Datum domainname_in(PG_FUNCTION_ARGS);
Datum domainname_in(PG_FUNCTION_ARGS)
{
	const char *in = PG_GETARG_CSTRING(0);
	size_t len = strlen(in);
	PG_RETURN_TEXT_P(domainname_new(in, len));
}

PG_FUNCTION_INFO_V1(domainname_read);
Datum domainname_read(PG_FUNCTION_ARGS);
Datum domainname_read(PG_FUNCTION_ARGS)
{
	text *in = PG_GETARG_TEXT_P(0);
	text *out = domainname_new(VARDATA(in), VARSIZE_ANY_EXHDR(in));
	PG_FREE_IF_COPY(in, 0);
	PG_RETURN_TEXT_P(out);
}

PG_FUNCTION_INFO_V1(domainname_out);
Datum domainname_out(PG_FUNCTION_ARGS);
Datum domainname_out(PG_FUNCTION_ARGS)
{
	text *in = PG_GETARG_TEXT_P(0);
	size_t len = VARSIZE_ANY_EXHDR(in);
	char *out = palloc(len+1);
	domainname_flip(out, VARDATA(in), len);
	out[len] = '\0';
	PG_FREE_IF_COPY(in, 0);
	PG_RETURN_CSTRING(out);
}

PG_FUNCTION_INFO_V1(domainname_show);
Datum domainname_show(PG_FUNCTION_ARGS);
Datum domainname_show(PG_FUNCTION_ARGS)
{
	text *in = PG_GETARG_TEXT_P(0);
	size_t len = VARSIZE_ANY(in);
	text *out = (text *)palloc(len);
	SET_VARSIZE(out, len);
	domainname_flip(VARDATA(out), VARDATA(in), len - VARHDRSZ);
	PG_FREE_IF_COPY(in, 0);
	PG_RETURN_TEXT_P(out);
}

#if 0
PG_FUNCTION_INFO_V1(domainname_parents);
Datum domainname_parents(PG_FUNCTION_ARGS);
Datum domainname_parents(PG_FUNCTION_ARGS)
{
	FuncCallContext *funcctx;
	text *in = PG_GETARG_TEXT_P(0);
	const char *s = VARDATA(in);
	const char *p = s, *e = s + VARSIZE_ANY_EXHDR(in);
	unsigned i;

	if (SRF_IS_FIRSTCALL())
		funcctx = SRF_FIRSTCALL_INIT();

	funcctx = SRF_PERCALL_SETUP();

	for (i = 0; i < funcctx->call_cntr && ++p < e; i ++)
		STRSEARCH(p, e-p, *p == '.');

	if (i == funcctx->call_cntr)
		SRF_RETURN_NEXT(funcctx, PointerGetDatum(cstring_to_text_with_len(s, p-s)));
	else
		SRF_RETURN_DONE(funcctx);
}
#else
PG_FUNCTION_INFO_V1(domainname_parents);
Datum domainname_parents(PG_FUNCTION_ARGS);
Datum domainname_parents(PG_FUNCTION_ARGS)
{
	text *in = PG_GETARG_TEXT_P(0);
	const char *s = VARDATA(in);
	const char *p = s, *e = s + VARSIZE_ANY_EXHDR(in);
	int nelems = 1;
	int nbytes = ARR_OVERHEAD_NONULLS(1) + VARHDRSZ;
	ArrayType *r;
	char *o;

	while (p < e) 
	{
		STRSEARCH(p, e-p, *p == '.');
		nelems ++;
		nbytes += VARHDRSZ + (p-s);
		nbytes = INTALIGN(nbytes);
		p ++;
	}
	r = (ArrayType *)palloc(nbytes);
	SET_VARSIZE(r, nbytes);
	r->ndim = 1;
	r->dataoffset = 0;
	r->elemtype = get_fn_expr_argtype(fcinfo->flinfo, 0);
	*ARR_DIMS(r) = nelems;
	*ARR_LBOUND(r) = 0;

	p = s;
	o = ARR_DATA_PTR(r);
	SET_VARSIZE(o, VARHDRSZ);
	o = VARDATA(o);
	while (p < e)
	{
		STRSEARCH(p, e-p, *p == '.');
		SET_VARSIZE(o, VARHDRSZ+(p-s));
		o = VARDATA(o);
		memcpy(o, s, p-s);
		o += INTALIGN(p-s);
		p ++;
	}

	PG_FREE_IF_COPY(in, 0);
	PG_RETURN_ARRAYTYPE_P(r);
}
#endif

PG_FUNCTION_INFO_V1(domainname_parts);
Datum domainname_parts(PG_FUNCTION_ARGS);
Datum domainname_parts(PG_FUNCTION_ARGS)
{
	text *in = PG_GETARG_TEXT_P(0);
	const char *s = VARDATA(in);
	const char *b = s, *p = s, *e = s + VARSIZE_ANY_EXHDR(in);
	int nelems = 0;
	int nbytes = ARR_OVERHEAD_NONULLS(1);
	ArrayType *r;
	char *o;

	while (p < e) 
	{
		b = p;
		STRSEARCH(p, e-p, *p == '.');
		nelems ++;
		nbytes += VARHDRSZ + (p-b);
		nbytes = INTALIGN(nbytes);
		p ++;
	}
	r = (ArrayType *)palloc(nbytes);
	SET_VARSIZE(r, nbytes);
	r->ndim = 1;
	r->dataoffset = 0;
	r->elemtype = TEXTOID;
	*ARR_DIMS(r) = nelems;
	*ARR_LBOUND(r) = 1;

	p = s;
	o = ARR_DATA_PTR(r);
	while (p < e)
	{
		b = p;
		STRSEARCH(p, e-p, *p == '.');
		SET_VARSIZE(o, VARHDRSZ+(p-b));
		o = VARDATA(o);
		memcpy(o, b, p-b);
		o += INTALIGN(p-b);
		p ++;
	}

	PG_FREE_IF_COPY(in, 0);
	PG_RETURN_ARRAYTYPE_P(r);
}


struct uri_info {
	const char *scheme;
	int scheme_len;
	const char *host;
	int host_len;
	int port;
	const char *path;
	int path_len;
};

#define PORT_LEN	7

static bool isdelim(char c)
{
	return c == ':' || c == '/' || c == '@' || c == '?' || c == '#';
}

static bool uri_parse(const char *str, size_t len, struct uri_info *uri)
{
	const char *b, *p = str, *e = str+len, *x;
#define NEXT(I) ({ \
		b = p += (I); \
		STRSEARCH(p, e-p, isdelim(*p)); \
	})

	NEXT(0);

	if (p < e && p[0] == ':' && !(
				(x = strndigit(p+1, MIN(e-p-1,PORT_LEN))) > p+1 && 
				(x == e || isdelim(*x))))
	{
		uri->scheme = b;
		uri->scheme_len = p-b;
		NEXT(1);
	}
	else
	{
		uri->scheme = NULL;
		uri->scheme_len = 0;
	}

	if (p < e-1 && p == b && p[0] == '/' && p[1] == '/')
		NEXT(2);

	if (p < e-1 && *p == '@')
		NEXT(1);

	uri->host = b;
	uri->host_len = p-b;
	uri->port = -1;

	while (p < e-1 && p[0] == ':')
	{
		char portbuf[8];
		unsigned portlen = sizeof(portbuf)-1;

		NEXT(1);

		if (p-b < portlen)
			portlen = p-b;
		memcpy(portbuf, b, portlen);
		portbuf[portlen] = 0;

		uri->port = strtoul(portbuf, (char **)&x, 10);
		if (!*x)
			break;

		uri->host_len = p-uri->host;
		uri->port = -1;
	}

	if (p < e)
	{
		uri->path = p;
		uri->path_len = e-p;
	}
	else
	{
		uri->path = NULL;
		uri->path_len = 0;
	}

#undef NEXT

	return true;
}

enum uri_tuple {
	URI_HOST = 0,
	URI_PORT,
	URI_PATH,
	URI_SCHEME,

	URI_LEN
};

static HeapTuple uri_new(FunctionCallInfo fcinfo, const char *str, size_t len)
{
	TupleDesc td;
	struct uri_info u;
	Datum d[URI_LEN];
	bool n[URI_LEN];

	if (!uri_parse(str, len, &u))
		ereport(ERROR, (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
					errmsg("invalid uri: \"%.*s\"", (int)len, str)));

	if (!(n[URI_SCHEME] = !u.scheme))
		d[URI_SCHEME] = PointerGetDatum(cstring_to_text_with_len(u.scheme, u.scheme_len));
	if (!(n[URI_HOST] = !u.host))
		d[URI_HOST] = PointerGetDatum(domainname_new(u.host, u.host_len));
	if (!(n[URI_PORT] = u.port < 0))
		d[URI_PORT] = Int16GetDatum(u.port);
	if (!(n[URI_PATH] = !u.path))
		d[URI_PATH] = PointerGetDatum(cstring_to_text_with_len(u.path, u.path_len));
	get_call_result_type(fcinfo, NULL, &td);
	return heap_form_tuple(BlessTupleDesc(td), d, n);
}

PG_FUNCTION_INFO_V1(uri_in);
Datum uri_in(PG_FUNCTION_ARGS);
Datum uri_in(PG_FUNCTION_ARGS)
{
	const char *str = PG_GETARG_CSTRING(0);
	PG_RETURN_DATUM(HeapTupleGetDatum(uri_new(fcinfo, str, strlen(str))));
}

PG_FUNCTION_INFO_V1(uri_read);
Datum uri_read(PG_FUNCTION_ARGS);
Datum uri_read(PG_FUNCTION_ARGS)
{
	text *in = PG_GETARG_TEXT_P(0);
	HeapTuple out = uri_new(fcinfo, VARDATA(in), VARSIZE_ANY_EXHDR(in));
	PG_FREE_IF_COPY(in, 0);
	PG_RETURN_DATUM(HeapTupleGetDatum(out));
}

static void *uri_char(HeapTupleHeader ud, bool hdr, bool term)
{
	TupleDesc td;
	HeapTupleData tuple;
	Datum d[URI_LEN];
	bool n[URI_LEN];
	text *scheme = NULL, *host = NULL, *path = NULL;
	int16 port;
	char portbuf[8];
	unsigned schemelen = 0, hostlen = 0, portlen = 0, pathlen = 0;
	unsigned len;
	void *out;
	char *p;

	td = lookup_rowtype_tupdesc(HeapTupleHeaderGetTypeId(ud), HeapTupleHeaderGetTypMod(ud));
	tuple.t_len = HeapTupleHeaderGetDatumLength(ud);
	ItemPointerSetInvalid(&(tuple.t_self));
	tuple.t_tableOid = InvalidOid;
	tuple.t_data = ud;
	heap_deform_tuple(&tuple, td, d, n);
	ReleaseTupleDesc(td);

	if (!n[URI_SCHEME])
	{
		scheme = DatumGetTextP(d[URI_SCHEME]);
		schemelen = VARSIZE_ANY_EXHDR(scheme);
	}
	if (!n[URI_HOST])
	{
		host = DatumGetTextP(d[URI_HOST]);
		hostlen = VARSIZE_ANY_EXHDR(host);
	}
	if (!n[URI_PORT])
	{
		port = DatumGetInt16(d[URI_PORT]);
		portlen = snprintf(portbuf, sizeof(portbuf)-1, ":%hu", port);
	}
	if (!n[URI_PATH])
	{
		path = DatumGetTextP(d[URI_PATH]);
		pathlen = VARSIZE_ANY_EXHDR(path);
	}

	len = (hdr ? VARHDRSZ : 0) + schemelen + (scheme ? 3 : 0) + hostlen + portlen + pathlen + term;
	out = palloc(len);
	if (hdr)
		SET_VARSIZE(out, len);
	p = hdr ? VARDATA(out) : out;

	if (scheme)
	{
		memcpy(p, VARDATA(scheme), schemelen);
		p += schemelen;
		*p++ = ':';
		*p++ = '/';
		*p++ = '/';
	}
	if (host)
	{
		domainname_flip(p, VARDATA(host), hostlen);
		p += hostlen;
	}
	memcpy(p, portbuf, portlen);
	p += portlen;
	if (path)
	{
		memcpy(p, VARDATA(path), pathlen);
		p += pathlen;
	}
	if (term)
		*p = '\0';

	return out;
}

PG_FUNCTION_INFO_V1(uri_out);
Datum uri_out(PG_FUNCTION_ARGS);
Datum uri_out(PG_FUNCTION_ARGS)
{
	HeapTupleHeader ud = PG_GETARG_HEAPTUPLEHEADER(0);
	PG_RETURN_CSTRING(uri_char(ud, 0, 1));
}

PG_FUNCTION_INFO_V1(uri_show);
Datum uri_show(PG_FUNCTION_ARGS);
Datum uri_show(PG_FUNCTION_ARGS)
{
	HeapTupleHeader ud = PG_GETARG_HEAPTUPLEHEADER(0);
	PG_RETURN_TEXT_P(uri_char(ud, 1, 0));
}

void _PG_init(void);
void _PG_init()
{
}
