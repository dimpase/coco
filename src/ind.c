/*
 * COCO - Unix version 1.0 - aeb - 920530
 *
 * ind.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include "def.h"
#include "prt.h"
#include "ingp.h"
#include "outgp.h"
#include "common.h"
#include "readstr.h"

struct descriptor {
    char tag;
#define OUTER 0
#define SEQ 1
#define SET 2
#define CYCL 3
#define DBL 4
    unsigned int k;
    union {
	struct element {
	    unsigned int l;
	    struct descriptor *d;
	} el[1];
	struct {
	    unsigned int m, u[1];
	} cycl;
    } inf;
};

#define NPMAX 5500
#define NOMAX 15             /* max number of orbits on structure */
#define BNDMAX 32000
#define NDMAX 1000            /* max total size of descriptors (in chars) */
#define NHASH 256
#define NSTR 32000           /* max length of string (input line) */

jmp_buf jb;
struct file ingpf, instrucf, outgpf, outmapf;

#define fig ingpf.fd
#define fis instrucf.fd
#define fog outgpf.fd
#define fom outmapf.fd
#define flig ingpf.flags
#define flis instrucf.flags
#define flog outgpf.isopen
#define flom outmapf.isopen
int f;

static int element(str, lstr, n, x, ep, k, poserr)
    char str[];
    unsigned int lstr, n, *x, *ep, *k, *poserr;
{
    char s;
    unsigned int j;

    if (lstr == 0)
	*k = 0;
    else {
	if (x > ep) {
	    prt(3, "number of points is greater than %d\n", NPMAX);
	    longjmp(jb, 1);
	}
	for (*k = 1, *x = j = 0; j < lstr; j++)
	    if ((s = str[j]) < '0' || s > '9' ||
		(*x = 10 * (*x) + (s - '0')) >= n) {
		*poserr = j;
		return 0;
	    }
    }
    return 1;
}

static int compset(str1, lstr1, str2, lstr2, n, point, ep, poserr)
    char str1[], str2[];
    unsigned int lstr1, lstr2, n, point[], *ep, *poserr;
{
    char s1, s2;
    unsigned int i, j1, j2, t;
    int u;

    for (u = i = j1 = j2 = 0; j1 < lstr1 && j2 < lstr2; j1++, j2++, i += t) {
	if ((s1 = str1[j1]) == '[')
	    u++;
	if (s1 == ']')
	    u--;
	for (; u == 0 && j1 < lstr1 && (s1 = str1[j1]) >= '0' && s1 <= '9';
	     j1++);
	if (&point[i] >= ep) {
	    prt(3, "number of points is greater than %d\n", NPMAX);
	    longjmp(jb, 1);
	}
	for (point[i] = t = 0;
	     u == 0 && j2 < lstr2 && (s2 = str2[j2]) >= '0' && s2 <= '9';
	     t = 1, j2++)
	    if ((point[i] = 10 * point[i] + (s2 - '0')) >= n) {
		*poserr = j2;
		return 0;
	    }
	if (j1 < lstr1 && j2 < lstr2 && str1[j1] != str2[j2]) {
	    *poserr = j2;
	    return 0;
	}
    }
    if (j1 < lstr1 || j2 < lstr2) {
	*poserr = j2;
	return 0;
    }
    return 1;
}

static int descrcycl(str, lstr, n, descr, enddescr, point, ep, descr1,
		     np, poserr)
    char str[];
    unsigned int lstr, n, *enddescr, point[], *ep, *np, *poserr, **descr1;
    struct descriptor *descr;
{
    char s;
    unsigned int j, r, m, u;

    if (lstr < 2 || str[1] != '(') {
	*poserr = 1;
	return 0;
    }
    for (*np = r = 0, j = 2; j < lstr; j++)
	if (s = str[j], !r) {
	    if (&point[*np] >= ep) {
		prt(3, "number of points is greater than %d\n", NPMAX);
		longjmp(jb, 1);
	    }
	    if (s < '0' || s > '9' || (point[*np] = s - '0') >= n) {
		*poserr = j;
		return 0;
	    } else
		r = 1;
	} else if (s == ',') {
	    (*np)++;
	    r = 0;
	} else {
	    if (s == ')') {
		(*np)++;
		break;
	    }
	    if (s < '0' || s > '9' ||
		(point[*np] = 10 * point[*np] + (s - '0')) >= n) {
		*poserr = j;
		return 0;
	    }
	}
    if (j + 1 >= lstr) {
	*poserr = lstr - 1;
	return 0;
    }
    if (str[j + 1] != '[') {
	*poserr = j + 1;
	return 0;
    }
    for (m = r = 0, j = j + 2; j < lstr; j++)
	if (s = str[j], !r)
	    if (s < '0' || s > '9' || (u = s - '0') >= *np) {
		*poserr = j;
		return 0;
	    } else
		r = 1;
	else if (s == ',' || s == ']') {
	    if (!u) {
		*poserr = j - 1;
		return 0;
	    }
	    if (r = 0, u > 1) {
		if (&(descr->inf.cycl.u[m]) >= enddescr) {
		    prt(3, "excessively long descriptor\n");
		    longjmp(jb, 1);
		}
		descr->inf.cycl.u[m++] = u;
	    }
	    if (s == ']')
		break;
	} else if (s < '0' || s > '9' || (u = 10 * u + (s - '0')) >= *np) {
	    *poserr = j;
	    return 0;
	}
    if (j != lstr - 1) {
	*poserr = j;
	return 0;
    }
    descr->tag = CYCL;
    descr->k = *np;
    descr->inf.cycl.m = m;
    *descr1 = &(descr->inf.cycl.u[m]);
    return(1);
}

static int prepdescr(char str[], unsigned int lstr, unsigned int n,
                     struct descriptor *descr, void *enddescr,
                     unsigned int point[], unsigned int *ep,
                     struct descriptor **descr1,
                     unsigned int *np, unsigned int *poserr);

static int descrset(str, lstr, n, descr, enddescr, point, ep,
		    descr1, np, poserr)
    char str[];
    unsigned int lstr, n, *enddescr, point[], *ep, *np, *poserr;
    struct descriptor *descr, **descr1;
{
    char s;
    unsigned int i, j, k, l, ld, ld0, bs, bs0, np1, poserr1;
    int u;
    struct descriptor *pd;

    if (lstr < 2) {
	*poserr = lstr;
	return 0;
    }
    if (str[l = lstr - 1] != '}') {
	*poserr = l;
	return 0;
    }
    for (u = 0, k = (l == 1) ? 0 : 1, j = 1; j < l; j++) {
	if ((s = str[j]) == '(' || s == '{' || s == '[')
	    u++;
	if (s == ')' || s == '}' || s == ']')
	    u--;
	if (s == ',' && u == 0)
	    k++;
    }
    descr->tag = SET;
    descr->k = k;
    pd = (struct descriptor *) &(descr->inf.el[1]);
    *descr1 = pd;
    for (*np = i = 0, j = 1; i < k; i++, j++, pd = *descr1, (*np) += np1) {
	for (u = ld = 0, bs = j; j < l; ld++, j++) {
	    if ((s = str[j]) == '(' || s == '{' || s == '[')
		u++;
	    if (s == ')' || s == '}' || s == ']')
		u--;
	    if (s == ',' && u == 0)
		break;
	}
	if (!i)
	    if ((s = str[bs]) == '(' || s == '{' || s == 'C') {
		if (!prepdescr(&str[bs0 = bs], (ld0 = ld), n, pd, enddescr,
			       &point[*np], ep, descr1,
			       &np1, &poserr1)) {
		    *poserr = bs + poserr1;
		    return 0;
		}
		descr->inf.el[0].l = np1;
		descr->inf.el[0].d = pd;
	    } else {
		if (!element(&str[bs0 = bs], (ld0 = ld), n, &point[*np],
			     ep, &np1, &poserr1)) {
		    *poserr = bs + poserr1;
		    return 0;
		}
		if (&(descr->inf.el[0].l) >= enddescr) {
		    prt(3, "excessively long descriptor\n");
		    longjmp(jb, 1);
		}
		descr->inf.el[0].l = np1;
		*descr1 = pd;
	    }
	else if (!compset(&str[bs0], ld0, &str[bs], ld, n, &point[*np],
			  ep, &poserr1)) {
	    *poserr = bs + poserr1;
	    return 0;
	}
    }
    return 1;
}

static int descrdbl(str, lstr, n, descr, enddescr, point, ep, descr1, np, poserr)
    char str[];
    unsigned int lstr, n, *enddescr, point[], *ep, *np, *poserr;
    struct descriptor *descr, **descr1;
{
    char s;
    unsigned int i, j, k, l, ld, ld0, bs, bs0, np1, poserr1, *d;
    int u;
    struct descriptor *pd;

    if (lstr < 2) {
	*poserr = lstr;
	return 0;
    }
    if (str[l = lstr - 1] != '"') {
	*poserr = l;
	return 0;
    }
    if (str[1] != '{') {
	*poserr = 1;
	return 0;
    }
    for (; l > 1 && str[l] != '}'; l--);
    if (l == 1) {
	*poserr = lstr - 1;
	return 0;
    }
    for (u = 0, k = 1, j = 2; j < l; j++) {
	if ((s = str[j]) == '(' || s == '{' || s == '[')
	    u++;
	if (s == ')' || s == '}' || s == ']')
	    u--;
	if (s == ',' && u == 0)
	    k++;
    }
    descr->tag = DBL;
    descr->k = k;
    pd = (struct descriptor *) &(descr->inf.el[2]);
    for (*np = i = 0, j = 2; i < k; i++, j++, pd = *descr1, (*np) += np1) {
	for (u = ld = 0, bs = j; j < l; ld++, j++) {
	    if ((s = str[j]) == '(' || s == '{' || s == '[')
		u++;
	    if (s == ')' || s == '}' || s == ']')
		u--;
	    if (s == ',' && u == 0)
		break;
	}
	if (!i)
	    if ((s = str[bs]) == '(' || s == '{' || s == 'C') {
		if (!prepdescr(&str[bs0 = bs], (ld0 = ld), n, pd, enddescr,
			       &point[*np], ep, descr1,
			       &np1, &poserr1)) {
		    *poserr = bs + poserr1;
		    return 0;
		}
		descr->inf.el[0].l = np1;
		descr->inf.el[0].d = pd;
	    } else {
		*poserr = bs;
		return 0;
	    }
	else if (!compset(&str[bs0], ld0, &str[bs], ld, n, &point[*np], ep,
			  &poserr1)) {
	    *poserr = bs + poserr1;
	    return 0;
	}
    }
    d = &(descr->inf.el[1].l);
    *d = 0;
#define MAXINT 100000      /* no restriction intended? */
    if (lstr - l - 2)
	if (!element(str + l + 1, lstr - l - 2, MAXINT, d, d, &np1, &poserr1)){
	    *poserr = l + poserr1 + 1;
	    return 0;
	}
    return 1;
}

static int inpdescr(n, descr, enddescr, point, nnmax, poserr)
    unsigned int n, point[], nnmax[], *poserr;
    struct descriptor *descr;
    void *enddescr;
{
    unsigned int i, j, j1, k, bp, np, ld, ld1, m, *ep, bs, poserr1;
    struct descriptor *pd, *pd1;
    char str[NSTR];
    char s;

    read1str(fis, flis, str, NSTR);

    for (k = 1, j = 0; (s = str[j]); j++)
	if (s == '/')
	    k++;
    if (k > NOMAX) {
	prt(3, "number of orbits is greater than %d\n", NOMAX);
	longjmp(jb, 1);
    }

    descr->tag = OUTER;
    descr->k = k;
    pd = (struct descriptor *) &(descr->inf.el[k]);
    ep = point + NPMAX;
    for (m = bp = i = j = 0; i < k;
	 i++, j++, pd = pd1, bp += np, m += nnmax[i]) {
	for (ld = 0, bs = j; str[j] && str[j] != '/'; ld++, j++);
	if (!ld) {
	    prt(3, "empty string?\n");
	    *poserr = j;
	    return 0;
	}
	for (j1 = bs + (ld1 = ld) - 1;
	     j1 > bs && (s = str[j1]) >= '0' && s <= '9'; ld1--, j1--);
	if (!prepdescr(&str[bs], ld1, n, pd, enddescr,
		       &point[bp], ep, &pd1, &np, &poserr1)) {
	    *poserr = bs + poserr1;
	    return 0;
	}
	for (nnmax[i] = 0, j1 = bs + ld1; ld1 < ld; ld1++, j1++)
	    if ((nnmax[i] = 10 * nnmax[i] + (str[j1] - '0')) > BNDMAX - m) {
		*poserr = j1;
		return 0;
	    }
	descr->inf.el[i].l = np;
	descr->inf.el[i].d = pd;
    }
    return 1;
}

static void input(ggen, descr, enddescr, point, nnmax)
    struct ggen *ggen;
    struct descriptor *descr;
    void *enddescr;
    unsigned int *point, nnmax[];
{
    unsigned int poserr;

    readgp(&ingpf, ggen);
    while (1) {
	if (flis)
	    prt(3, "inducing on structure:\n");
	if (inpdescr(ggen->n, descr, enddescr, point, nnmax, &poserr))
	    break;
	prt(3, "error in position %d\n", poserr);
	if (!(flis & KBDBIT))
	    longjmp(jb, 1);
    }
    if (flis)
	prt(3, "\n");
}

static int descrseq(str, lstr, n, descr, enddescr, point,
		    ep, descr1, np, poserr)
    char str[];
    unsigned int lstr, n, *enddescr, point[], *ep, *np, *poserr;
    struct descriptor *descr, **descr1;
{
    char s;
    unsigned int i, j, k, l, ld, bs, np1, poserr1;
    int u;
    struct descriptor *pd;

    if (lstr < 2) {
	*poserr = lstr;
	return 0;
    }
    if (str[l = lstr - 1] != ')') {
        prt(3, "seq does not end with ')'\n");
	*poserr = l;
	return 0;
    }
    for (u = 0, k = (l == 1) ? 0 : 1, j = 1; j < l; j++) {
	if ((s = str[j]) == '(' || s == '{' || s == '[')
	    u++;
	if (s == ')' || s == '}' || s == ']')
	    u--;
	if (s == ',' && u == 0)
	    k++;
    }
    descr->tag = SEQ;
    descr->k = k;
    pd = (struct descriptor *) &(descr->inf.el[k]);
    *descr1 = pd;
    for (*np = i = 0, j = 1; i < k; i++, j++, pd = *descr1, (*np) += np1) {
	for (u = ld = 0, bs = j; j < l; ld++, j++) {
	    if ((s = str[j]) == '(' || s == '{' || s == '[')
		u++;
	    if (s == ')' || s == '}' || s == ']')
		u--;
	    if (s == ',' && u == 0)
		break;
	}
	if ((s = str[bs]) == '(' || s == '{' || s == 'C') {
	    if (!prepdescr(&str[bs], ld, n, pd, enddescr,
			   &point[*np], ep, descr1, &np1, &poserr1)) {
		*poserr = bs + poserr1;
		return 0;
	    }
	    descr->inf.el[i].l = np1;
	    descr->inf.el[i].d = pd;
	} else {
	    if (!element(&str[bs], ld, n, &point[*np], ep, &np1, &poserr1)) {
		*poserr = bs + poserr1;
		return 0;
	    }
	    if (&(descr->inf.el[i].l) >= enddescr) {
		prt(3, "excessively long descriptor\n");
		longjmp(jb, 1);
	    }
	    descr->inf.el[i].l = np1;
	    *descr1 = pd;
	}
    }
    return 1;
}

static int prepdescr(char str[], unsigned int lstr, unsigned int n,
		     struct descriptor *descr, void *enddescr,
		     unsigned int point[], unsigned int *ep,
		     struct descriptor **descr1,
		     unsigned int *np, unsigned int *poserr)
{
    int res;

    if (&(descr->k) >= (unsigned int *) enddescr) {
	prt(3, "excessively long descriptor\n");
	longjmp(jb, 1);
    }
    switch (str[0]) {
    case '(':
	res = descrseq(str, lstr, n, descr, enddescr,
			 point, ep, descr1, np, poserr);
	if(!res)
	  prt(3, "sequence descr error\n");
	return (res);
    case '{':
	res = descrset(str, lstr, n, descr, enddescr,
			 point, ep, descr1, np, poserr);
	if(!res)
	  prt(3, "set descr error\n");
	return (res);
    case 'C':
	res = descrcycl(str, lstr, n, descr, enddescr,
			  point, ep, descr1, np, poserr);
	if(!res)
	  prt(3, "cycl descr error\n");
	return(res);
    case '"':
	res = descrdbl(str, lstr, n, descr, enddescr,
			 point, ep, descr1, np, poserr);
	if(!res)
	  prt(3, "dbl descr error\n");
	return(res);
    default:
	*poserr = 0;
	return 0;
    }
}

static void prorbs(unsigned int n, unsigned int k, unsigned int lorb[])
{
    int l, i;
    char str[81];

    if (k == 1)
	prt(3, "\ninduced transitive group on %d points\n", n);
    else {
	Sprintf(str, "\ninduced group on %d points with %d orbits of length ",
		n, k);
	for (i = 0; i < k; i++) {
	    if ((l = strlen(str)) > 73) {
		prt(3, "%s\n", str);
		l = 0;
	    }
	    if (i < k - 1)
		Sprintf(str + l, "%d,", lorb[i]);
	    else
		Sprintf(str + l, "%d", lorb[i]);
	}
	prt(3, "%s\n", str);
    }
}

static void canoncycl(k, point, m, u)
    unsigned int k, point[], m, u[];
{
    unsigned int i, j, min, s, r, imin, umin, cycl[NPMAX];

    for (min = point[0], imin = 0, i = 1; i < k; i++)
	if ((s = point[i]) < min) {
	    min = s;
	    imin = i;
	}
    for (min = point[(imin + 1) % k], umin = 1, j = 0; j < m; j++)
	if ((s = point[(imin + (r = u[j])) % k]) < min) {
	    min = s;
	    umin = r;
	}
    for (i = 0; i < k; i++)
	cycl[i] = point[(imin + i * umin) % k];
    for (i = 0; i < k; i++)
	point[i] = cycl[i];
}

static void orders(k, point)
    unsigned int k, *point;
{
    unsigned int p1, p2, *pp1, *pp2, *pe;

    for (pe = (pp1 = point) + (k - 1); pp1 < pe; pp1++)
	for (p1 = *pp1, pp2 = pp1 + 1; pp2 <= pe; pp2++)
	    if (p1 > (p2 = *pp2)) {
		*pp2 = p1;
		p1 = *pp1 = p2;
	    }
}

static void order(k, l, point)
    unsigned int k, l, *point;
{
    unsigned int **ps1, **ps2, **pes, *pp1, *pp2, *pe1, *pe2, *pee;
    unsigned int *s[NPMAX / 2], r[NPMAX / 2];

    for (pes = (ps1 = s) + k, pp1 = point; ps1 < pes; ps1++, pp1 += l)
	*ps1 = pp1;
    for (pes = (ps1 = s) + (k - 1); ps1 < pes; ps1++)
	for (ps2 = ps1 + 1; ps2 <= pes; ps2++) {
	    for (pe1 = *ps1, pe2 = *ps2; *pe1 == *pe2; pe1++, pe2++);
	    if (*pe1 > *pe2) {
		pp1 = *ps1;
		*ps1 = *ps2;
		*ps2 = pp1;
	    }
	}
    for (pes = (ps1 = s) + k, pp1 = point; ps1 < pes; ps1++, pp1 += l)
	if (*ps1 != NULL && *ps1 != pp1) {
	    for (pee = (pe1 = pp1) + l, pe2 = r; pe1 < pee; pe1++, pe2++)
		*pe2 = *pe1;
	    for (ps2 = ps1, pp2 = pp1; *ps2 != pp1;
		 pp2 = *ps2, *ps2 = NULL, ps2 = s + (pp2 - point) / l)
		for (pee = (pe1 = *ps2) + l, pe2 = pp2; pe1 < pee;
		     pe1++, pe2++)
		    *pe2 = *pe1;
	    for (*ps2 = NULL, pee = (pe1 = r) + l, pe2 = pp2;
		 pe1 < pee; pe1++, pe2++)
		*pe2 = *pe1;
	}
}

static void canon(descr, point)
    unsigned int *point;
    struct descriptor *descr;
{
    unsigned int i, k, l, b;

    k = descr->k;
    switch (descr->tag) {
    case SEQ:
	for (b = i = 0; i < k; i++, b += l)
	    if ((l = descr->inf.el[i].l) > 1)
		canon(descr->inf.el[i].d, point + b);
	break;
    case SET:
	if ((l = descr->inf.el[0].l) > 1) {
	    for (b = i = 0; i < k; i++, b += l)
		canon(descr->inf.el[0].d, point + b);
	    order(k, l, point);
	} else
	    orders(k, point);
	break;
    case CYCL:
	canoncycl(k, point, descr->inf.cycl.m, descr->inf.cycl.u);
    }
}

unsigned int 
numpoint(np, point, nnmax, nn, hashhead, hashlist, points)
    unsigned int np, *point, nnmax, *nn, *hashhead, *hashlist, *points;
{
    unsigned int f, i, l, i1, hashf();

    f = hashf(np, point);
    l = np * sizeof(*point);
    for (i1 = i = *(hashhead + f); i != nnmax; i = *(hashlist + i))
        if (bcmp((char *) (points + np * i), (char *) point, (int) l) == 0)
	    break;
    if (i != nnmax)
	return (i);
    if (*nn < nnmax) {
	bcopy((char *) point, (char *) (points + np * (*nn)), (int) l);
	*(hashhead + f) = *nn;
	*(hashlist + *nn) = i1;
    }
    return ((*nn)++);
}

unsigned int 
hashf(np, point)
    unsigned np, point[];
{
    unsigned int i, i1, s, s1;

    for (i = s = 0; i < np; i++) {
	i1 = (i & 15);
	s1 = point[i];
	s += (s1 << i1) + (s1 >> (16 - i1));
    }
    return ((((s * 0x983F69B1) & 0xFFF000) >> 12) % NHASH);
}

static int newpoint(n, k, d, point, m, hashhead, hashlist, points, point1)
    unsigned int n, k, m, *point, *hashhead, *hashlist, *points,
    *point1;
    struct descriptor *d;
{
    unsigned int i, l, *pp, *pp1, *ep;

    l = k * sizeof(*point);
    for (pp = point, ep = (pp1 = point1) + n; pp1 < ep; pp1++, pp += k) {
	canon(d, pp);
	for (i = *(hashhead + hashf(k, pp)); i != m; i = *(hashlist + i))
	    if (bcmp((char *) (points +  k * i), (char *) pp, (int) l) == 0)
		break;
	if (i == m)
	    return 0;
	*pp1 = i;
    }
    return 1;
}

static void outpoint(struct descriptor *descr,
		     unsigned int *point, unsigned int *points,
		     char *str, unsigned int *lc)
{
    int ls, i, k, l, b;
    struct descriptor *d;

    ls = strlen(str);
    k = descr->k;
    switch (descr->tag) {
    case SEQ:
	Sprintf(str + (ls++), "(");
	for (b = i = 0; i < k; i++, b += l) {
	    if (i) {
		Sprintf(str + (ls++), ",");
		ls = partline(str, *lc, fom);
		*lc = ls;
	    }
	    if ((l = descr->inf.el[i].l) > 1)
		outpoint(descr->inf.el[i].d, point + b, points, str, lc);
	    else
		Sprintf(str + ls, "%u", *(point + b));
	    ls = strlen(str);
	}
	Sprintf(str + (ls++), ")");
	break;
    case SET:
	Sprintf(str + (ls++), "{");
	for (l = descr->inf.el[0].l, d = descr->inf.el[0].d, b = i = 0;
	     i < k; i++, b += l) {
	    if (i) {
		Sprintf(str + (ls++), ",");
		ls = partline(str, *lc, fom);
		*lc = ls;
	    }
	    if (l > 1)
		outpoint(d, point + b, points, str, lc);
	    else
		Sprintf(str + ls, "%u", *(point + b));
	    ls = strlen(str);
	}
	Sprintf(str + (ls++), "}");
	break;
    case CYCL:
	Sprintf(str + ls, "C(");
	ls += 2;
	for (i = 0; i < k; i++) {
	    if (i) {
		Sprintf(str + (ls++), ",");
		ls = partline(str, *lc, fom);
		*lc = ls;
	    }
	    Sprintf(str + ls, "%u", *(point + i));
	    ls = strlen(str);
	}
	Sprintf(str + ls, ")[1");
	ls += 3;
	ls = partline(str, *lc, fom);
	*lc = ls - 2;
	for (l = descr->inf.cycl.m, i = 0; i < l; i++) {
	    Sprintf(str + (ls++), ",");
	    ls = partline(str, *lc, fom);
	    *lc = ls;
	    Sprintf(str + ls, "%u", descr->inf.cycl.u[i]);
	    ls = strlen(str);
	}
	Sprintf(str + (ls++), "]");
	break;
    case DBL:
	Sprintf(str + (ls++), "{");
	for (l = descr->inf.el[0].l, d = descr->inf.el[0].d, i = 0;
	     i < k; i++) {
	    if (i) {
		Sprintf(str + (ls++), ",");
		ls = partline(str, *lc, fom);
		*lc = ls;
	    }
	    outpoint(d, points + l * (*(point + i)), points, str, lc);
	    ls = strlen(str);
	}
	Sprintf(str + (ls++), "}");
    }
    (void) partline(str, *lc, fom);
}

static void outmap(struct descriptor *d, unsigned int n, unsigned int k,
		   unsigned int n0, unsigned int *p, unsigned int *p1)
{
    char str[100];
    unsigned int i, lc;

    for (i = 0; i < n; i++, p += k) {
	Sprintf(str, "%5u.", i + n0);
	outpoint(d, p, p1, str, &lc);
	Fprintf(fom, "%s\n", str);
    }
}

static void writgens(unsigned int n, unsigned int l, char *buf)
{
    int x, y;

    for (x = n * l; x; x -= y) {
	if (write(f, buf, (y = (x > 60000) ? 60000 : x)) <= 0) {
	    prt(3, "output error on work file ind#1.tmp : %s\n",
		strerror(errno));
	    longjmp(jb, 1);
	}
	buf += y;
    }
}

static void readgens(unsigned int n, unsigned int l, char *buf)
{
    int x, y;

    lseek(f, 0l, SEEK_SET);
    for (x = (long) n * l; x; x -= y) {
	read0(f, buf, (y = (x > 60000) ? 60000 : x));
	buf += y;
    }
}

void cleanup()
{
    if (!(flig & KBDBIT))
	(void) fclose(fig);
    if (!(flis & KBDBIT))
	(void) fclose(fis);
    if (flom)
	(void) fclose(fom);
    if (flog) {
	(void) fclose(fog);
	(void) close(f);
	(void) unlink("ind#1.tmp");
    }
}

static int induce(ng, gens, descr, np, point, n0, nnmax, nn, gensn,
		  points, hashhead, hashlist)
    unsigned int ng, gens[], np, *point, n0, nnmax, *nn, *gensn, *points;
    unsigned int *hashhead, *hashlist;
    struct descriptor *descr;
{
    unsigned int i, ig, j, point1[NPMAX], num, *pp, *pg, numpoint();

    for (*nn = i = 0; i < NHASH; i++)
	hashhead[i] = nnmax;
    canon(descr, point);
    numpoint(np, point, nnmax, nn, hashhead, hashlist, points);
    for (pp = points, pg = gensn, i = 0; i < *nn; i++, pp += np) {
	for (ig = 0; ig < ng; ig++) {
	    for (j = 0; j < np; j++)
		point1[j] = gens[ig + ng * (*(pp + j))];
	    canon(descr, point1);
	    if ((num = numpoint(np, point1, nnmax, nn, hashhead, hashlist,
				points)) == nnmax)
		return 0;
	    *(pg++) = n0 + num;
	}
    }
    return 1;
}

static void ind(ng, gens, descr, point, nnmax)
    unsigned int ng, *gens, *point, nnmax[];
    struct descriptor *descr;
{
    unsigned int k, i, b, nn, nno, np, np1, m, l, lg, lp, lh;
    unsigned int *points, *points1, *gensn, *gensn1;
    unsigned int lorb[NOMAX], hashhead[NHASH], point1[NPMAX / 2], *hashlist;
    struct descriptor *d, *d1;

    k = descr->k;
    lg = ng * sizeof(*gensn);
    lh = sizeof(*hashlist);
    for (nn = b = i = 0; i < k; i++, b += np, nn += nno) {
	if ((d = descr->inf.el[i].d)->tag != DBL) {
	    l = lg + (lp = (np1 = np = descr->inf.el[i].l) * sizeof(*points))
	      + lh;
	    if ((m = nnmax[i]) == 0) {
	        /* size of orbit unknown - just allocate a lot of space */
		m = BNDMAX - nn;
	    }
	    if ((gensn = (unsigned int *) getmem(m, l)) == NULL) {
		prt(3, "not enough space for inducing of %d orbit\n", i + 1);
		longjmp(jb, 1);
	    }
	    hashlist = (points = gensn + m * ng) + m * np;
	    if (!induce(ng, gens, d, np, point + b, nn, m, &nno, gensn,
			points, hashhead, hashlist)) {
		prt(3, "length of %d orbit is greater than %d\n", i + 1, m);
		longjmp(jb, 1);
	    }
	} else {
	    lp = (np = d->inf.el[0].l) * sizeof(*points1);
	    if ((m = d->inf.el[1].l) == 0)
		m = BNDMAX;
	    if ((points1 = (unsigned int *) getmem(m, lp + lh)) == NULL
		|| (gensn1 = (unsigned int *) getmem(m, lg)) == NULL) {
		prt(3, "not enough space for first inducing of %d orbit\n",
		    i + 1);
		longjmp(jb, 1);
	    }
	    hashlist = points1 + m * np;
	    if (!induce(ng, gens, (d1 = d->inf.el[0].d),
			np, point + b, 0, m, &nno, gensn1,
			points1, hashhead, hashlist)) {
		prt(3, "length of orbit of first inducind for %d orbit ",
		    i + 1);
		prt(3, "is greater than %d\n", m);
		longjmp(jb, 1);
	    }
	    prt(3, "length of orbit of first inducing for %d orbit is %d\n",
		i + 1, nno);
	    if (!newpoint(d->k, np, d1, point + b, m, hashhead, hashlist,
			  points1, point1)) {
		prt(3,
		    "elements of structure for induce %d orbit do not belong ",
		    i + 1);
		prt(3, "to one orbit\n");
		longjmp(jb, 1);
	    }
	    if (!flom)
		free((char *) points1);
	    l = lg + (lp = (np1 = d->k) * sizeof(*points)) + lh;
	    if ((m = nnmax[i]) == 0)
		m = BNDMAX - nn;
	    if ((gensn = (unsigned int *) getmem(m, l)) == NULL) {
		prt(3, "not enough space for inducing of %d orbit\n", i + 1);
		longjmp(jb, 1);
	    }
	    hashlist = (points = gensn + m * ng) + m * np1;
	    d->tag = SET;
	    d->inf.el[0].l = 1;
	    if (!induce(ng, gensn1, d, np1, point1, nn, m, &nno, gensn, points,
			hashhead, hashlist)) {
		prt(3, "length of %d orbit is greater than %d\n", i + 1, m);
		longjmp(jb, 1);
	    }
	    d->inf.el[0].l = np;
	    d->tag = DBL;
	    np = descr->inf.el[i].l;
	    free((char *) gensn1);
	}
	lorb[i] = nno;
	if (flog && k > 1) {
	    writgens(nno, lg, (char *) gensn);
	    free(gensn);
	}
	if (!flog)
	    free(gensn);
	if (flom) {
	    outmap(d, nno, np1, nn, points, points1);
	    if (d->tag == DBL)
		free(points1);
	}
	prt(3, "length of orbit %d is %d\n", i + 1, nno);
    }
    prorbs(nn, k, lorb);
    if (flog) {
	if (k > 1) {
	    if ((gensn = (unsigned int *) getmem(nn, lg)) == NULL) {
		prt(3, "not enough space for output of generators\n");
		longjmp(jb, 1);
	    }
	    readgens(nn, lg, (char *) gensn);
	}
	outgens(nn, ng, gensn, fog);
	free((char *) gensn);
    }
}

static void openf(int na, char *pa[])
{
    if(!openfile("input of group from %s\n",
		 &ingpf, (na > 2) ? pa[2] : "*", "gen", "rt"))
	longjmp(jb, 1);

    if(!openfile("input of structure for inducing from %s\n",
		 &instrucf, (na > 3) ? pa[3] : "*", "str", "rt"))
	longjmp(jb, 1);

    if(na > 4) {
      if(!openfile("output of induced group to file %s\n",
		   &outgpf, pa[4], "gen", "wt"))
	longjmp(jb, 1);
	f = open("ind#1.tmp", O_RDWR | O_CREAT, 0666);
    } else
	flog = 0;

    if (na > 5) {
      if(!openfile("output of mapping to file %s\n",
		   &outmapf, pa[5], "map", "wt"))
	longjmp(jb, 1);
    } else
	flom = 0;

    prt(3, "\n");
}

void Main(int na, char **pa)
{
    unsigned int nnmax[NOMAX], *point;
    char descr[NDMAX];
    struct ggen ggen;

    prt(3, "\n* induced action of permutation group on structure *\n\n");
    openf(na, pa);
    if((point = Getmem(NPMAX, unsigned int)) == NULL) {
        prt(3, "Out of memory\n");
	longjmp(jb, 1);
    }
    input(&ggen, (struct descriptor *) descr, descr + NDMAX, point, nnmax);
    ind(ggen.ng, ggen.gens, descr, point, nnmax);
}
