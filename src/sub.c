/*
 * COCO - Unix version 1.0 - aeb - 920530
 *
 * sub.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <string.h>
#include <errno.h>
#include "def.h"
#include "prt.h"
#include "common.h"
#include "readstr.h"

#define BMAX 15
#define twopow 256
#define RMAX twopow-1
#define NSMAX 1000
#define NSUBMAX 500
#define LEXT 16000


jmp_buf jb;
struct file innrsf, inoptf, outsubf;
#define fic innrsf.fd
#define fio inoptf.fd
#define fos outsubf.fd
#define flic innrsf.flags
#define flio inoptf.flags
#define flos outsubf.isopen

struct {
    unsigned int k, *p;
} set[NSMAX];

struct {
    unsigned int r, k;
    unsigned int *p;
} sub[NSUBMAX];

static int inpsym(b, r, ref, na, sym, poserr)
    unsigned int b, r, *ref, *na, *poserr;
    int *sym;
{
    unsigned int i, x, y, u[2];
    int s;

    readstr(fic, flic);
    for (i = 0; i < r; i++)
	sym[i] = i;
    for (i = 0; i < b; i++)
	sym[ref[i]] = -1;
    for (*na = x = i = 0; (s = nxtsym()); i++)
	switch (x) {
	case 0:
	    if (s == '(') {
		x = 1;
		y = 0;
		break;
	    }
	    *poserr = i;
	    return 0;
	case 1:
	    if (s > '0' && s <= '9' && (u[y] = s - '0') < r) {
		x = 2;
		break;
	    }
	    *poserr = i;
	    return 0;
	case 2:
	    if (s == ',' && !y) {
		x = 1;
		y = 1;
		break;
	    }
	    if (s == ')') {
		for (y = 0; y < 2; y++) {
		    if (sym[u[y]] != u[y]) {
			prt(3, "repeated information on class %d\n", u[y]);
			*poserr = i;
			return 0;
		    }
		    sym[u[y]] = u[1 - y];
		}
		x = 0;
		(*na)++;
		break;
	    }
	    if (s < '0' || s > '9' || (u[y] = 10 * u[y] + (s - '0')) >= r) {
		*poserr = i;
		return 0;
	    }
	}
    *poserr = i;
    return ((x) ? 0 : 1);
}

static void setrfm(b, rnk, matr, rfm)
    unsigned int b, *rnk, *matr, **rfm;
{
    unsigned int b1, b2, b3;

    for (b1 = 0; b1 < b; b1++)
	for (b2 = 0; b2 < b; b2++)
	    for (b3 = 0; b3 < b; b3++) {
		*(rfm++) = (unsigned int *) matr;
		matr += ((long) *(rnk + b * b1 + b2)) * (*(rnk + b * b1 + b3))
			* (*(rnk + b * b2 + b3));
	    }
}

static int inpmatr(b, r, rnk, beg, end, d, matr, rfm, n, poserr)
    unsigned int b, r, *rnk, *beg, *end, *d, *rfm[], *matr, *n, *poserr;
{
    unsigned int i, x, y, u[3], *pm, b2, al, bt, gm, u0, u1, u2;
    int s;
    long r2;

    readstr(fic, flic);
    for (r2 = ((long) r) * r, b2 = b * b, x = *n = i = 0;
	 (s = nxtsym()); i++)
	switch (x) {
	case 0:
	    if (s == '(') {
		x = 1;
		y = 0;
		break;
	    }
	    *poserr = i;
	    return 0;
	case 1:
	    if (s >= '0' && s <= '9' && (u[y] = s - '0') < r) {
		x = 2;
		break;
	    }
	    *poserr = i;
	    return 0;
	case 2:
	    if (s == ',' && y < 2) {
		x = 1;
		y++;
		break;
	    }
	    if (s == '-' && y == 2) {
		x = 3;
		break;
	    }
	    if (s < '0' || s > '9' || (u[y] = 10 * u[y] + (s - '0')) >= r) {
		*poserr = i;
		return 0;
	    }
	    break;
	case 3:
	    if (s > '0' && s <= '9') {
		y = s - '0';
		x = 4;
		break;
	    }
	    *poserr = i;
	    return 0;
	case 4:
	    if (s == ')') {
		if (b == 1)
		    pm = matr + (r2 * u[0] + r * u[1] + u[2]);
		else {
		    if ((al = beg[u0 = u[0]]) != beg[u2 = u[2]] ||
			(bt = beg[u1 = u[1]]) != end[u0] ||
			(gm = end[u1]) != end[u2]) {
			prt(3, "intersection number p(%u,%u,%u) does not exist\n",
			    u0, u1, u2);
			*poserr = i;
			return 1;
		    }
		    pm = *(rfm + b2 * al + b * bt + gm) +
			(((long) rnk[b * al + gm]) *
			 (rnk[b * bt + gm] * d[u0] + d[u1])
			 + d[u2]);
		}
		if (*pm) {
		    prt(3, "repeated intersection number p(%d,%d,%d)\n",
			u[0], u[1], u[2]);
		    *poserr = i;
		    return 0;
		}
		*pm = y;
		(*n)++;
		x = 0;
		break;
	    }
	    if (s < '0' || s > '9') {
		*poserr = i;
		return 0;
	    }
	    y = 10 * y + (s - '0');
	}
    return ((x) ? 0 : 1);
}

static int tprim(r, matr, l, set)
    unsigned int r, * matr, l;
    unsigned int *set;
{
    unsigned int orb[RMAX], v, w;
    unsigned int i, i1, j, k, m, * pi, * pj, * pk, s;
    long r2;

    for (r2 = ((long) r) * r, i = 0; i < r; i++)
	orb[i] = i;
    for (m = r, i1 = 0; i1 < l; i1++)
	for (i = set[i1], j = 0; j < r; j++)
	    for (pi = matr + (r2 * i + r * j),
		 pj = matr + (r2 * j + r * i),
		 pk = matr + (r2 * j + i), k = 0;
		 k < r; k++)
		if (orb[j] != orb[k] && (*(pi + k) || *(pj + k) || *(pk + r * k))) {
		    if ((--m) == 1)
			return 1;
		    for (w = orb[j], v = orb[k], s = 0; s < r; s++)
			if (orb[s] == w)
			    orb[s] = v;
		}
    return 0;
}

static unsigned int prm(r, matr, prim)
    unsigned int r, *matr, *prim;
{
    unsigned int i, s;

    for (s = 0, i = 1; i < r; i++) {
	prim[i] = tprim(r, matr, 1, &i);
	if (!prim[i]) {
	    if (!s) {
		prt(3, "disconnected classes: %u", i);
		s = 1;
	    } else
		prt(3, ",%u", i);
	}
    }
    if (s)
	prt(3, "\n");
    return (s);
}

static void listsym(r, sym, n, list)
    unsigned int r, *n, *list;
    int *sym;
{
    int i, s;

    for (*n = 0, i = 0; i < r; i++) {
	if ((s = sym[i]) == i)
	    list[(*n)++] = i;
	if (s > i) {
	    list[(*n)++] = i | twopow;
	    list[(*n)++] = s;
	}
    }
}

static void listasym(b, r, sym, n, list)
    unsigned int b, r, *n, *list;
    int *sym;
{
    int i, s;

    for (*n = 0, i = 1; i < r; i++)
	if ((s = sym[i]) > i || (b > 1 && s >= 0 && s != i))
	    list[(*n)++] = i;
}

unsigned int *p, *ep;
int nset;

static void setup()
{

    nset = 0;
    p = ep = NULL;
}

static void inset(k, s)
    unsigned int k;
    unsigned int *s;
{
    if (nset + 1 == NSMAX) {
	prt(3, "number of good sets is greater than %d\n", NSMAX);
	longjmp(jb, 1);
    }
    if (p + k >= ep) {
   if ((p = (unsigned int *) getmem(LEXT, 1)) == NULL) {
	    prt(3, "not enough space for good sets\n");
	    longjmp(jb, 1);
	}
	ep = p + LEXT;
    }
    set[nset].p = p;
    set[nset++].k = k;
    bcopy((int *)s, (int *)p, (int)k * sizeof(*s));
    p += k;
}

unsigned int *p1, *ep1, nsub;

static void setup1()
{

    nsub = 0;
    p1 = ep1 = NULL;
}

static void insub(r, k, s)
    unsigned int r, k, *s;
{
    if (nsub + 1 == NSUBMAX) {
	prt(3, "number of subschemes is greater than %d\n", NSMAX);
	longjmp(jb, 1);
    }
    if (p1 + k >= ep1) {
	if ((p1 = (unsigned int *) getmem(LEXT, 1)) == NULL) {
	    prt(3, "not enough space for subschemes\n");
	    longjmp(jb, 1);
	}
	ep1 = p1 + LEXT;
    }
    sub[nsub].p = p1;
    sub[nsub].r = r;
    sub[nsub++].k = k;
    bcopy((int *)s, (int *)p1, (int)k * sizeof(*s));
    p1 += k;
}

static void goodsets(mode, u, n, list, r, sym, matr, ns)
    unsigned char mode;
    unsigned int u, n, *list, r, *matr, *ns;
    int *sym;
{
    unsigned int q, q1, ls, ls0, x, y, z, i, j, s, s0, *pm, *pm1, *pmm;
    unsigned int set[RMAX], pr;
    int k, l;
    long r2, lambda, ss, ss0;
    struct {
	unsigned int s0, s1;
	unsigned int l, q, z, pr;
	int k;
    } lev[RMAX];

    r2 = r * r;
    for (*ns = l = lev[0].q = lev[0].l = lev[0].z = lev[0].pr = 0,
	 lev[0].k = -1;;
	 lev[l].q = q1, lev[l].z = z, lev[++l].l = ls, lev[l].q = q,
	 lev[l].z = 0, lev[l].pr = pr) {
	for (; l >= 0 && (q = lev[l].q) == n; l--);
	if (l < 0)
	    break;
	ls = lev[l].l;
	pr = lev[l].pr;
	z = lev[l].z;
        for (q1 = q, x = 1; x; ls++, q++, x = y & twopow) {
            set[ls] = (y = list[q]) & (twopow-1);
	    if (z)
		set[ls] = sym[set[ls]];
	}
	if (u && l && !z)
	    z = 1;
	else {
	    z = 0;
	    q1 = q;
	}
	if (ls > 1 && (u || ls < n)) {
	    for (k = 0; k < ls; k++) {
		pm = matr + set[k];
		if (k <= lev[l].k) {
		    ls0 = lev[l].l;
		    s = (k < lev[l].k) ? lev[l].s0 : lev[l].s1;
		    for (i = 0; i < ls0; i++) {
		        pm1 = (unsigned int *) (pm + r2 * set[i]);
			for (j = ls0; j < ls; j++)
			    s += *(pm1 + r * set[j]);
		    }
		} else
		    ls0 = s = 0;
		for (i = ls0; i < ls; i++) {
		    pm1 = (unsigned int *) (pm + r2 * set[i]);
		    for (j = 0; j < ls; j++)
			s += *(pm1 + r * set[j]);
		}
		if (!k)
		    lev[l + 1].s0 = s0 = s;
		else if (s != s0)
		    break;
	    }
	    lev[l + 1].k = (k == ls) ? (k - 1) : k;
	    lev[l + 1].s1 = s;
	    if (k == ls) {
		for (k = 0; k < ls; k++) {
		    for (pmm = (pm = matr) + set[k], ss = x = 0; x < r;
			 x++, pmm += r2, pm++, ss += lambda * s) {
			for (lambda = y = 0; y < ls; y++)
			    lambda += *(pmm + r * set[y]);
			if (lambda)
			    for (s = i = 0; i < ls; i++) {
			        pm1 = (unsigned int *) (pm + r2 * set[i]);
				for (j = 0; j < ls; j++)
				    s += *(pm1 + r * set[j]);
			    }
		    }
		    if (!k)
			ss0 = ss;
		    else if (ss != ss0)
			break;
		}
		if (k == ls && (!mode || pr || tprim(r, matr, ls, set))) {
		    pr = 1;
		    inset(ls, set);
		    (*ns)++;
		}
	    }
	} else
	    lev[l + 1].k = -1;
    }
}

static int tprimrf(b, r, rnk, beg, end, d, rfm, l, set)
    unsigned int b, r, *rnk, *beg, *end, *d, * rfm[], l;
    unsigned int *set;
{
    unsigned int orb[RMAX], v, w;
    unsigned int i, i1, j, k, m, s, b2, al, bt, gm;

    for (i = 0; i < r; i++)
	orb[i] = i;
    for (b2 = b * b, m = r, i1 = 0; i1 < l; i1++)
	for (i = set[i1], j = 0; j < r; j++)
	    for (k = 0; k < r; k++)
		if (orb[j] != orb[k])
		    if (((al = beg[i]) == beg[k] && (bt = beg[j]) == end[i] &&
			 (gm = end[j]) == end[k]
			 && *(*(rfm + b2 * al + b * bt + gm) +
			      (((long) rnk[b * al + gm]) * (rnk[b * bt + gm] *
						     d[i] + d[j]) + d[k]))) ||
			((al = beg[j]) == beg[k] && (bt = beg[i]) == end[j] &&
			 (gm = end[i]) == end[k]
			 && *(*(rfm + b2 * al + b * bt + gm) +
			      (((long) rnk[b * al + gm]) * (rnk[b * bt + gm] *
						     d[j] + d[i]) + d[k]))) ||
			((al = beg[j]) == beg[i] && (bt = beg[k]) == end[j] &&
			 (gm = end[k]) == end[i]
			 && *(*(rfm + b2 * al + b * bt + gm) +
			      (((long) rnk[b * al + gm]) * (rnk[b * bt + gm] *
						     d[j] + d[k]) + d[i])))) {
			if ((--m) == 1)
			    return 1;
			for (w = orb[j], v = orb[k], s = 0; s < r; s++)
			    if (orb[s] == w)
				orb[s] = v;
		    }
    return 0;
}

static void ordlist(no, n, list, r, rnk, beg, val, delta, c)
    unsigned int no, n, *list, r, *rnk, *beg, *val, *delta, *c;
{
    unsigned int i, j, k, b, ne, l, v, x, y, list1[RMAX], rr[BMAX], c1[BMAX];
    unsigned int s[RMAX];
    struct {
	unsigned int b, l, bg;
	unsigned int v;
    } el[RMAX];

    for (i = 0; i < no; i++)
	for (c1[i] = i, rr[i] = j = 0; j < no; j++)
	    rr[i] += rnk[i * no + j];
    for (i = 0; i < no - 1; i++)
	for (j = i + 1; j < no; j++)
	    if (rr[i] > rr[j]) {
		k = c1[i];
		c1[i] = c1[j];
		c1[j] = k;
		k = rr[i];
		rr[i] = rr[j];
		rr[j] = k;
	    }
    for (i = 0; i < no; i++)
	c[c1[i]] = i;
    for (ne = i = 0; i < n; el[ne].l = l, el[ne++].v = v) {
	for (k = BMAX, j = i, x = 1; x; x = y & twopow, j++)
	    if (k > (x = c[beg[(y = list[j]) & (twopow-1)]]))
		k = x;
	for (el[ne].b = i, el[ne].bg = k, l = v = 0, x = 1;
	     x; x = y & twopow, i++, l++)
	    if (c[beg[x = (y = list[i]) & (twopow-1)]] == k)
		v += val[x];
    }
    for (i = 0; i < ne; i++)
	s[i] = i;
    for (i = 0; i < ne - 1; i++)
	for (j = i + 1; j < ne; j++)
	    if (el[x = s[i]].bg > el[y = s[j]].bg ||
		(el[x].bg == el[y].bg && el[x].v > el[y].v)) {
		s[i] = y;
		s[j] = x;
	    }
    for (j = i = 0; i < ne; i++)
	for (b = el[x = s[i]].b, l = el[x].l, k = 0; k < l; k++, b++, j++)
	    list1[j] = list[b];
    for (i = 0; i < r; i++)
	delta[i] = 0;
    for (i = ne - 1; i > 0; i--)
	if (el[x = s[i]].bg == el[y = s[i - 1]].bg)
	    delta[list[el[y].b] & (twopow-1)] = delta[list[el[x].b] & (twopow-1)] + el[x].v;
    bcopy((int *)list1, (int *)list, (int)n * sizeof(*list));
}

static void goodhmrf(mode, u, n, list, b, r, rnk, beg, end, d, sym, rfm, ns)
    unsigned char mode;
    unsigned int u, n, *list, b, r, *rnk, *beg, *end, *d, * rfm[],
    *ns;
    int *sym;
{
    unsigned int q, q1, ls, x, y, z, i, j, k, s, s0, * pm, r1, c[BMAX],
     v[BMAX];
    unsigned int val[RMAX], delta[RMAX], b2, al, bt, gm, wk, wi, wj, w1,
     w2, w3;
    unsigned int set[RMAX], pr;
    int l;
    long wl;
    struct {
	unsigned int l, q, pr;
	unsigned int val[BMAX];
    } lev[RMAX];

    for (b2 = b * b, i = 0; i < r; i++) {
	val[i] = 1;
	if (sym[i] > 0) {
	    al = beg[i];
	    bt = end[i];
	    val[i] = *(*(rfm + (b2 + 1) * al + b * bt) +
		       (((long) rnk[(b + 1) * al]) *
			(rnk[b * bt + al] * d[i] + d[sym[i]])));
	}
    }
    for (r1 = i = 0; i < r; i++)
	if (beg[i] == 0 && sym[i] > 0)
	    r1 += val[i];
    ordlist(b, n, list, r, rnk, beg, val, delta, c);
    for (*ns = l = lev[0].q = lev[0].l = lev[0].pr = 0;;
       lev[l].q = q1, lev[++l].l = ls, lev[l].q = q, lev[l].pr = pr) {
	for (; l >= 0 && (q = lev[l].q) == n; l--);
	if (l < 0)
	    break;
	for (i = 0; i < b; i++)
	    lev[l].val[i] = (l) ? lev[l - 1].val[i] : 0;
	for (j = BMAX, ls = lev[l].l, pr = lev[l].pr, x = 1;
	     x && q < n; q++, x = y & twopow) {
	    set[ls] = x = (y = list[q]) & (twopow-1);
	    if (u)
		for (z = sym[x], k = 0; k < ls && set[k] != z; k++);
	    if (!u || k == ls) {
		k = c[beg[i = set[ls++]]];
		lev[l].val[k] += val[i];
		if (j > k)
		    j = k;
	    } else
		y = twopow;
	}
	if (x)
	    q1 = n;
	else {
	    for (q1 = q, k = lev[l].val[0], s = 0, i = 1; i < b && s < 2; i++)
		if ((x = lev[l].val[i]) != k) {
		    if (i < j)
			s = 3;
		    if (i == j) {
			if (x > k)
			    s = (lev[l - 1].val[i] < k) ? 3 : 2;
			else if (x < k)
			    s = (x + delta[set[lev[l].l]] < k) ? 3 : 1;
		    }
		    if (i > j)
			s = (x > k && j > 1) ? 2 : 1;
		}
	    if (s > 1 || k == r1) {
		q = n;
		if (s == 3)
		    q1 = n;
	    } else if (!s) {
		if (u) {
		    for (x = y = BMAX, k = 0; k < ls; k++) {
			if ((z = set[k]) < x)
			    x = z;
			if ((z = sym[z]) < y)
			    y = z;
		    }
		    if (x < y) {
			for (i = 0; i < b; i++)
			    v[i] = 0;
			for (i = 0; i < ls; i++) {
			    k = sym[set[i]];
			    v[beg[k]] += val[k];
			}
			for (i = 1; i < b && v[i] == *v; i++);
		    }
		}
		if (!u || (x < y && i == b)) {
		    for (k = 0; k < ls; k++) {
			al = beg[wk = set[k]];
			gm = end[wk];
			w1 = b2 * al + gm;
			wl = rnk[b * al + gm];
			w2 = d[wk];
			for (s = i = 0; i < ls; i++)
			    if (beg[wi = set[i]] == al)
				for (bt = end[wi], pm = *(rfm + w1 + b * bt),
				     w3 = rnk[b * bt + gm] * d[wi], j = 0;
				     j < ls; j++)
				    if (beg[wj = set[j]] == bt && end[wj] == gm)
					s += *(pm + (wl * (w3 + d[wj]) + w2));
			if (!k)
			    s0 = s;
			else if (s != s0)
			    break;
		    }
		    if (k == ls && (!mode || pr ||
			     tprimrf(b, r, rnk, beg, end, d, rfm, ls, set))) {
			pr = 1;
			inset(ls, set);
			(*ns)++;
		    }
		}
	    }
	}
    }
}

static int testsr(b, r, rnk, beg, end, d, sym, matr, rfm, lp,
		  parts, ls, subs, ns)
    unsigned int b, r, *rnk, *beg, *end, *d, *matr, * rfm[],
     lp, *parts, ls, ns;
    int *sym;
    unsigned int *subs;
{
    unsigned int si, sj, sk, i, j, k, qi, qj, qk, s, s1, t, t1, x;
    unsigned int b2, al, bt, gm, ii, jj, kk, w1, w3;
    unsigned int *mi, *mi1, *mj, * mk, *mk1;
    unsigned int *pi, *pj, *pk, *pi1, *pj1, *pk1, *epi, *epj, *epk;
    long r2, w2;

    if (b == 1) {
      for (r2 = r * r, i = 0; i < lp; i++) {
	qi = parts[i];
	for (epi = (pi = set[qi].p) + set[qi].k, si = 0;
	     !si || ((si == 1) && (qi >= ns)); si++) {
	  for (j = 0; j < lp; j++) {
	    qj = parts[j];
	    for (epj = (pj = set[qj].p) + set[qj].k, sj = 0;
		 !sj || ((sj == 1) && (qj >= ns)); sj++) {
	      for (k = 0; k < lp; k++) {
		qk = parts[k];
		for (epk = (pk = set[qk].p) + set[qk].k, sk = 0;
		     !sk || ((sk == 1) && (qk >= ns)); sk++) {
		  for (pk1 = pk; pk1 < epk; pk1++) {
		    mk = matr + ((sk) ? sym[*pk1] : *pk1);
		    for (s = 0, pi1 = pi; pi1 < epi; pi1++) {
		      mi = (unsigned int *) (mk + r2 * ((si) ? sym[*pi1] : *pi1));
		      for (pj1 = pj; pj1 < epj; pj1++)
			s += *(mi + r * ((sj) ? sym[*pj1] : *pj1));
		    }
		    if (pk1 == pk)
		      s1 = s;
		    else if (s1 != s)
		      return (2);
		  }
		}
	      }
	    }
	  }
	}
      }
      for (i = 0; i < ls; i++) {
	mi1 = (unsigned int *) ((mk = matr) + r2 * subs[i]);
	for (j = 0; j < ls; j++) {
	  mj = mi1 + r * subs[j];
	  for (k = 0; k < lp; k++) {
	    qk = parts[k];
	    for (epk = (pk = set[qk].p) + set[qk].k, sk = 0;
		 !sk || ((sk == 1) && (qk >= ns)); sk++) {
	      for (s = *(mj + ((sk) ? sym[*pk] : *pk)), pk1 = pk + 1;
		   pk1 < epk; pk1++)
		if (s != *(mj + ((sk) ? sym[*pk1] : *pk1)))
		  return 1;
	    }
	  }
	}
	for (mi = matr + r * subs[i], j = 0; j < lp; j++) {
	  qj = parts[j];
	  for (epj = (pj = set[qj].p) + set[qj].k, sj = 0;
	       !sj || ((sj == 1) && (qj >= ns)); sj++) {
	    for (k = 0; k < lp; k++) {
	      qk = parts[k];
	      for (epk = (pk = set[qk].p) + set[qk].k, sk = 0;
		   !sk || ((sk == 1) && (qk >= ns)); sk++) {
		for (pk1 = pk; pk1 < epk; pk1++) {
		  x = (sk) ? sym[*pk1] : *pk1;
		  for (mk1 = mi1 + x, mk = mi + x, t = s = 0, pj1 = pj;
		       pj1 < epj; pj1++) {
		    s += *(mk1 + r * (x = (sj) ? sym[*pj1] : *pj1));
		    t += *(mk + r2 * x);
		  }
		  if (pk1 == pk) {
		    s1 = s;
		    t1 = t;
		  } else if (s1 != s || t1 != t)
		    return 1;
		}
	      }
	    }
	  }
	}
      }
    } else {
      for (b2 = b * b, i = 0; i < lp; i++) {
	qi = parts[i];
	for (epi = (pi = set[qi].p) + set[qi].k, si = 0;
	     !si || ((si == 1) && (qi >= ns)); si++) {
	  for (j = 0; j < lp; j++) {
	    qj = parts[j];
	    for (epj = (pj = set[qj].p) + set[qj].k, sj = 0;
		 !sj || ((sj == 1) && (qj >= ns)); sj++) {
	      for (k = 0; k < lp; k++) {
		qk = parts[k];
		for (epk = (pk = set[qk].p) + set[qk].k, sk = 0;
		     !sk || ((sk == 1) && (qk >= ns)); sk++) {
		  for (pk1 = pk; pk1 < epk; pk1++) {
		    al = beg[kk = (sk) ? sym[*pk1] : *pk1];
		    for (w1 = b2 * al + (gm = end[kk]),
			 w2 = rnk[b * al + gm],
			 w3 = d[kk], s = 0, pi1 = pi; pi1 < epi; pi1++)
		      if (al == beg[ii = (si) ? sym[*pi1] : *pi1])
			for (bt = end[ii], pj1 = pj; pj1 < epj; pj1++) {
			  jj = sj ? sym[*pj1] : *pj1;
			  if (bt == beg[jj] && gm == end[jj])
			    s += *(*(rfm + w1 + b * bt) +
				   (w2 * (rnk[b * bt + gm] * d[ii] + d[jj]) + w3));
			}
		    if (pk1 == pk)
		      s1 = s;
		    else if (s1 != s)
		      return (2);
		  }
		}
	      }
	    }
	  }
	}
      }
    }
    return 0;
}

/* sort the array sub[0..nsub-1] according to sub[*].r (small rank last) */
static void ordsub(nsub)
    unsigned int nsub;
{
    unsigned int i, j, im, *p;
    unsigned int s;

    if(nsub > 0)      /* necessary because of `unsigned' */
      for (im = nsub - 1, i = 0; i < im; i++)
	for (j = i + 1; j < nsub; j++)
	    if ((s = sub[i].r) < sub[j].r) {
		sub[i].r = sub[j].r;
		sub[j].r = s;
		s = sub[i].k;
		sub[i].k = sub[j].k;
		sub[j].k = s;
		p = sub[i].p;
		sub[i].p = sub[j].p;
		sub[j].p = p;
	    }
}

static void pr3(b, r, rnk, beg, end, d, sym, matr, rfm, parts, str)
    unsigned int b, r, *rnk, *beg, *end, *d, * matr, * rfm[];
    int *sym;
    unsigned int *parts;
    char *str;
{
    unsigned int i, j, k, v1, la1, gm, bt, w2;
    unsigned int *p, *p1, *p2, *ep;
    long r2, w1;
    unsigned int n, v, la;

    if (b == 1) {
	for (r2 = r * r, n = i = 1; i < r; i++)
	    n += *(matr + (r2 * i + r * sym[i]));
	v = la = 0;
	for (ep = (p = p1 = set[*parts].p) + set[*parts].k; p1 < ep; p1++) {
	    i = *p1;
	    for (v += *(matr + (r2 * i + r * sym[i])), p2 = p; p2 < ep; p2++)
		la += *(matr + (r2 * i + r * (*p2) + *p));
	}
    } else {
	for (w1 = *rnk, n = i = 1; i < r && !beg[i]; i++) {
	    w2 = b * end[i];
	    n += *(*(rfm + w2) + w1 * (rnk[w2] * d[i] + d[sym[i]]));
	}
	v = la = 0;
	for (ep = (p = p1 = set[*parts].p) + set[*parts].k,
	     w1 = rnk[gm = end[k = *p]];
	     p1 < ep; p1++)
	    if (!beg[i = *p1]) {
		w2 = b * end[i];
		v += *(*(rfm + w2) + w1 * (rnk[w2] * d[i] + d[sym[i]]));
		for (w2 = b * (bt = end[i]) + gm, p2 = p; p2 < ep; p2++)
		    if (beg[j = *p2] == bt && end[j] == gm)
			la += *(*(rfm + w2) + (w1 * (rnk[w2] * d[i] + d[j])
						+ d[k]));
	    }
    }
    v1 = n - v - 1;
    la1 = v1 - v + (((long) v) * (v - la - 1)) / v1 - 1;
    if (v1 < v || (v1 == v && la1 < la)) {
	v = v1;
	la = la1;
    }
    Sprintf(str + strlen(str), "with parameters (%u,%u,%u) ", n, v, la);
}

static void printsub(n, rs, lp, parts, b, r, rnk, beg, end, d, ref, sym, matr, rfm, ns)
    unsigned int n;
    unsigned char rs, lp;
    unsigned int *parts, b, r, *rnk, *beg, *end, *d, *ref, *matr, **rfm, ns;
    int *sym;
{
    unsigned int i, q, sm, ls, lc, lb;
    unsigned int *p, *p1, *ep;
    char str[100];

    Sprintf(str, "%u. subscheme of rank %u ", n, rs);
    if (rs == 3)
	pr3(b, r, rnk, beg, end, d, sym, matr, rfm, parts, str);
    Sprintf(str + strlen(str), "by merging ");
    if (b > 1) {
	for (ls = strlen(str), lb = i = 0; i < b; i++) {
	    if (!i) {
		Sprintf(str + ls, "(%u", *ref);
		lb = ls;
	    } else {
		Sprintf(str + ls, ",%u", ref[i]);
		lc = ls + 1;
	    }
	    if ((ls = strlen(str)) > 75) {
		if (!lb)
		    lb = lc;
		ls = partline(str,lb,stdout);
		lb = 0;
	    }
	}
	str[ls++] = ')';
	str[ls] = '\0';
    } else
	lb = 0;
    for (ls = strlen(str), i = 0; i < lp; i++) {
        q = parts[i];
	for (ep = (p = set[q].p) + set[q].k, sm = 0;
	     !sm || ((sm == 1) && (q >= ns)); sm++) {
	    for (p1 = p; p1 < ep; p1++) {
		if (p1 == p) {
		    Sprintf(str + ls, "(%u", (sm) ? sym[*p1] : *p1);
		    lb = ls;
		} else {
		    Sprintf(str + ls, ",%u", (sm) ? sym[*p1] : *p1);
		    lc = ls + 1;
		}
		if ((ls = strlen(str)) > 75) {
		    if (!lb)
			lb = lc;
		    ls = partline(str,lb,stdout);
		    lb = 0;
		}
	    }
	    str[ls++] = ')';
	    str[ls] = '\0';
	}
    }
    prt(3, "%s\n", str);
}

static void outsub(lp, parts, b, ref, sym, ns)
    unsigned int lp, *parts, b, *ref, ns;
    int *sym;
{
    unsigned int i, q, sm, ls, lc, lb;
    unsigned int *p, *p1, *ep;
    char str[100];

    Sprintf(str, "");
    if (b > 1) {
	for (ls = strlen(str), i = 0; i < b; i++) {
	    Sprintf(str + ls, i ? ",%u" : "(%u", ref[i]);
	    ls = strlen(str);
	}
	Sprintf(str + ls, ")");
    }
    for (ls = strlen(str), lb = i = 0; i < lp; i++) {
        q = parts[i];
	for (ep = (p = set[q].p) + set[q].k, sm = 0;
	     !sm || ((sm == 1) && (q >= ns)); sm++) {
	    for (p1 = p; p1 < ep; p1++) {
		if (p1 == p) {
		    Sprintf(str + ls, "(%u", (sm) ? sym[*p1] : *p1);
		    lb = ls;
		} else {
		    Sprintf(str + ls, ",%u", (sm) ? sym[*p1] : *p1);
		    lc = ls + 1;
		}
		if ((ls = strlen(str)) > 75) {
		    if (!lb)
			lb = lc;
		    ls = partline(str, lb, fos);
		    lb = 0;
		}
	    }
	    str[ls++] = ')';
	    str[ls] = 0;
	}
    }
    Fprintf(fos, "%s\n", str);
}

static void subr(reg, mode, b, r, rnk, beg, end, d, ref, sym, matr, rfm, ns, na, prim)
    unsigned char reg, mode;
    unsigned int b, r, *rnk, *beg, *end, *d, *ref, *matr, **rfm, ns, na,
    *prim;
    int *sym;
{
    unsigned int n, q, i, l, ls, x, nr, r1, lp, parts[RMAX];
    unsigned int sc[RMAX], subs[RMAX], lev[RMAX], *p, *ep;

    n = ns + na;
    for (i = 0; i < r; i++)
	sc[i] = 0;
    for (i = 0; i < b; i++)
	sc[ref[i]] = 1;
    setup1();
    for (nr = lev[0] = l = 0;;) {
	for (q = lev[l]; q < n; q++) {
	    for (ep = (p = set[q].p) + set[q].k; p < ep && !sc[*p]; p++);
	    if (p == ep) {
		for (parts[l] = q, p = set[q].p; p < ep; p++) {
		    sc[*p] = 1;
		    if (q >= ns)
			sc[sym[*p]] = 1;
		}
		for (ls = 0, i = 1; i < r; i++)
		    if (!sc[i]) {
			if ((reg || sym[i] == i) && (!mode || prim[i]))
			    subs[ls++] = i;
			else
			    break;
		    }
		if (i == r && (b == 1 || !ls)) {
		    if ((x = testsr(b, r, rnk, beg, end, d, sym, matr,
				    rfm, l + 1, parts, ls, subs, ns)) == 0) {
			for (r1 = r - b + 1, lp = l + 1, i = 0; i < lp; i++)
			    r1 -= (set[parts[i]].k - 1) * ((parts[i] >= ns) ? 2 : 1);
			insub(r1, lp, parts);
			nr++;
		    }
		    if (x <= 1)
			break;
		    else
			for (p = set[q].p; p < ep; p++) {
			    sc[*p] = 0;
			    if (q >= ns)
				sc[sym[*p]] = 0;
			}
		} else
		    break;
	    }
	}
	if (q < n) {
	    lev[l] = lev[l + 1] = ++q;
	    l++;
	} else {
	    if (!l) {
		ordsub(nr);
		if (flos)
		    outsub(0, (unsigned int *)0, 1,
			   (unsigned int *)0, (int *) 0, 0);
		for (i = 0; i < nr; i++) {
		    printsub(i + 1, sub[i].r, sub[i].k, sub[i].p,
			   b, r, rnk, beg, end, d, ref, sym, matr, rfm, ns);
		    if (flos)
			outsub(sub[i].k, sub[i].p, b, ref, sym, ns);
		}
		if (nr)
		    prt(3, "\n");
		prt(3, "%u ", nr);
		if (!reg)
		    prt(3, "symmetrical ");
		if (reg == 2)
		    prt(3, "antisymmetrical ");
		if (mode)
		    prt(3, "primitive ");
		prt(3, "subschemes\n");
		break;
	    }
	    q = lev[--l] - 1;
	    for (ep = (p = set[q].p) + set[q].k; p < ep; p++) {
		sc[*p] = 0;
		if (q >= ns)
		    sc[sym[*p]] = 0;
	    }
	}
    }
}

static void gsets(reg, mode, b, r, rnk, beg, end, d, sym, matr, rfm, ns, na)
    unsigned char reg, mode;
    unsigned int b, r, *rnk, *beg, *end, *d, *matr, **rfm, *ns, *na;
    int *sym;
{
    unsigned int nl, list[RMAX];

    setup();
    if (reg < 2) {
	listsym(r, sym, &nl, list);
	if (b == 1)
	    goodsets(mode, 0, nl, list, r, sym, matr, ns);
	else
	    goodhmrf(mode, 0, nl, list, b, r, rnk, beg, end, d, sym, rfm, ns);
	prt(3, "there exist %u good symmetrical", *ns);
	if (mode)
	    prt(3, " connected");
	prt(3, " sets\n");
    } else
	*ns = 0;
    if (reg) {
	listasym(b, r, sym, &nl, list);
	if (b == 1)
	    goodsets(mode, 1, nl, list, r, sym, matr, na);
	else
	    goodhmrf(mode, 1, nl, list, b, r, rnk, beg, end, d, sym, rfm, na);
	prt(3, "there exist %u pairs of good antisymmetrical", *na);
	if (mode)
	    prt(3, " connected");
	prt(3, " sets\n");
    } else
	*na = 0;
}

static int inprm(b, ns, na, p, reg, mode)
    unsigned int b, ns, na, p;
    unsigned char *reg, *mode;
{
    readstr(fio, flio);
    if (strequal("\n")) {
	*reg = (na) ? 1 : 0;
	*mode = 0;
	return 1;
    }
    if (strequal("s\n")) {
      if (na) {
	*reg = 0;
	*mode = 0;
	return 1;
      } else
	return 0;
    }
    if (strequal("a\n")) {
      if (na && !ns) {
	*reg = 2;
	*mode = 0;
	return 1;
      } else
	return 0;
    }
    if (strequal("p\n")) {
      if(b > 1 || p) {
	*reg = (na) ? 1 : 0;
	*mode = 1;
	return 1;
      } else
	return 0;
    }
    if (na && (b > 1 || p)) {
      if (strequal("sp\n")) {
	*reg = 0;
	*mode = 1;
	return 1;
      }
      if (strequal("ap\n") && !ns) {
	*reg = 2;
	*mode = 1;
	return 1;
      }
    }
    return 0;
}

static int inprn(b, r, rnk, beg, end, d, ref, poserr)
    unsigned int b, *r, *rnk, *beg, *end, *d, *ref, *poserr;
{
    unsigned int i, j, k, x, i1;
    char s;

    readstr(fic, flic);
    for (*r = x = k = j = i = 0; (s = nxtsym()); i++)
	switch (x) {
	case 0:
	    if (s > '0' && s <= '9') {
		rnk[j] = s - '0';
		x = 1;
		break;
	    }
	    *poserr = i;
	    return 0;
	case 1:
	    if (s == ',' && j + 1 < b * b) {
		if (j == k * (b + 1))
		    ref[k++] = *r;
		if (*r + rnk[j] <= RMAX) {
		    for (x = i1 = 0; i1 < rnk[j]; i1++, (*r)++) {
			beg[*r] = j / b;
			end[*r] = j % b;
			d[*r] = i1;
		    }
		    j++;
		    break;
		}
	    }
	    if (s >= '0' && s <= '9' && (rnk[j] = 10 * rnk[j] + (s - '0')) <= RMAX)
		break;
	    *poserr = i;
	    return 0;
	}
    ref[k] = *r;
    if (j + 1 != b * b || *r + rnk[j] > RMAX) {
	*poserr = i;
	return 0;
    }
    for (i1 = 0; i1 < rnk[j]; i1++, (*r)++) {
	beg[*r] = end[*r] = b - 1;
	d[*r] = i1;
    }
    return 1;
}

static void inpopt(b, ns, na, p, reg, mode)
    unsigned int b, ns, na, p;
    unsigned char *reg, *mode;
{
    if (na || b > 1 || p) {
	while (1) {
	    if (flio) {
		prt(3, "options(");
		if (b > 1 || p)
		    if (na)
			if (ns)
			    prt(3, "s,p,sp");
			else
			    prt(3, "s,a,p,sp,ap");
		    else
			prt(3, "p");
		else if (ns)
		    prt(3, "s");
		else
		    prt(3, "s,a");
		prt(3, "):\n");
	    }
	    if (inprm(b, ns, na, p, reg, mode))
		break;
	    prt(3, "error\n");
	    if (!(flio & KBDBIT))
		longjmp(jb, 1);
	}
	if (flio)
	    prt(3, "\n");
    } else {
	*reg = 0;
	*mode = 0;
    }
}

static void input(b, r, rnk, beg, end, d, ref, sym, matr, rfm, prim, reg, mode)
    unsigned int *b, *r, *rnk, *beg, *end, *d, *ref, **matr, ***rfm, *prim;
    int *sym;
    unsigned char *reg, *mode;
{
    unsigned int b1, b2, b3, i, nc, n, poserr, *pm, *em, p, na, ns;
    long l;

    while (1) {
      if (flic)
	prt(3, "number of blocks (up to %d):\n", BMAX);
      readstr(fic, flic);
      if (inpnum(b) && *b > 0 && *b <= BMAX)
	break;
      prt(3, "error\n");
      if (!(flic & KBDBIT))
	longjmp(jb, 1);
    }
    while (1) {
	if (flic)
	    prt(3, "ranks (up to %d):\n", RMAX);
	if (inprn(*b, r, rnk, beg, end, d, ref, &poserr) &&
	    *r > 0 && *r <= RMAX)
	    break;
	prt(3, "error in position %d\n", poserr);
	if (!(flic & KBDBIT))
	    longjmp(jb, 1);
    }
    while (1) {
	if (flic)
	    prt(3, "pairs of antisymmetrical classes:\n");
	if (inpsym(*b, *r, ref, &na, sym, &poserr))
	    break;
	prt(3, "error in position %d\n", poserr);
	if (!(flic & KBDBIT))
	    longjmp(jb, 1);
    }
    if (*b > 1) {
	if((*rfm = Getmem((*b) * (*b) * (*b), unsigned int *)) == NULL) {
	  prt(3, "Out of memory\n");
	  longjmp(jb, 1);
	}
	for (l = b1 = 0; b1 < *b; b1++)
	  for (b2 = 0; b2 < *b; b2++)
	    for (b3 = 0; b3 < *b; b3++)
	      l += ((long) *(rnk + (*b) * b1 + b2)) * (*(rnk + (*b) * b1 + b3)) *
		(*(rnk + (*b) * b2 + b3));
	i = (l + 15) / 16;
    } else
	l = ((long) *r) * (*r) * (*r);
    if ((*b == 1 && (*matr = Getmem((*r)*(*r)*(*r), unsigned int)) == NULL) ||
	(*b > 1 && (*matr = Getmem(16*i, unsigned int)) == NULL)) {
	prt(3, "not enough space for intersection matrix\n");
	longjmp(jb, 1);
    }
    if (*b > 1)
	setrfm(*b, rnk, *matr, *rfm);
    for (em = (pm = *matr) + l; pm < em; pm++)
	*pm = 0;
    for (b1 = 0; b1 < *b; b1++)
	for (b2 = 0; b2 < *b; b2++) {
	    while (1) {
	      if (flic)
		prt(3, "number of intersection numbers with i of type %u,%u:\n",
		    b1 + 1, b2 + 1);
	      readstr(fic, flic);
	      if (inpnum(&nc) && nc > 0)
		break;
	      prt(3, "error\n");
	      if (!(flic & KBDBIT))
		longjmp(jb, 1);
	    }
	    if (flic)
		prt(3, "intersection numbers with i of type %d,%d:\n",
		    b1 + 1, b2 + 1);
	    for (i = 0; i < nc; i += n)
		while (1) {
		    if (inpmatr(*b, *r, rnk, beg, end, d, *matr,
				*rfm, &n, &poserr))
			break;
		    prt(3, "error in position %d\n", poserr);
		    if (!(flic & KBDBIT))
			longjmp(jb, 1);
		}
	}
    if (flic)
	prt(3, "\n");
    ns = *r - *b - 2 * na;
    if (*b > 1) {
	if (!ns)
	    prt(3, "antisymmetrical ");
	prt(3, "configuration of rank %u with %u blocks\n\n", *r, *b);
    } else {
	if ((p = prm(*r, *matr, prim)) == 0)
	    prt(3, "primitive ");
	else
	    prt(3, "imprimitive ");
	if (!na)
	    prt(3, "symmetrical ");
	if (!ns)
	    prt(3, "antisymmetrical ");
	prt(3, "scheme of rank %u\n\n", *r);
    }
    inpopt(*b, ns, na, p, reg, mode);
}

void cleanup()
{
    if (flos)
        (void) fclose(fos);
}

static void openf(na, pa)
    char *pa[];
    int na;
{
    if(!openfile("input of intersection numbers from %s\n",
		 &innrsf, (na > 2) ? pa[2] : "*", "nrs", "rt"))
      longjmp(jb, 1);

    if(!openfile("input of options from %s\n",
		 &inoptf, (na > 3) ? pa[3] : "*", "opt", "rt"))
      longjmp(jb, 1);

    if (na > 4) {
	if(!openfile("output of subconfigurations to file %s\n",
		     &outsubf, pa[4], "sub", "wt"))
	    longjmp(jb, 1);
    } else
	flos = 0;
    prt(3, "\n");
}

void Main(na, pa)
    char **pa;
    int na;
{
    unsigned int b, r, ref[BMAX], nsym, nasym, *matr, prim[RMAX], rnk[BMAX * BMAX];
    unsigned int beg[RMAX], end[RMAX], d[RMAX], **rfm;
    int sym[RMAX];
    unsigned char reg, mode;

    prt(3, "\n* construction of subconfigurations *\n\n");
    openf(na, pa);
    input(&b, &r, rnk, beg, end, d, ref, sym, &matr, &rfm, prim, &reg, &mode);
    gsets(reg, mode, b, r, rnk, beg, end, d, sym, matr, rfm, &nsym, &nasym);
    subr(reg, mode, b, r, rnk, beg, end, d, ref, sym, matr, rfm, nsym, nasym, prim);
}
