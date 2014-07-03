/*
 * COCO - Unix version 1.0 - aeb - 920530
 *
 * aut.c
 */
#include <stdio.h>
#include <setjmp.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include "def.h"
#include "prt.h"
#include "common.h"
#include "readstr.h"

#define NOMAX 15
#define RMAX 255
#define NBMAX 150
#define NORD 10

jmp_buf jb;
struct file inmergf;
#define fis inmergf.fd
#define flis inmergf.flags
int fic, fw1, fw2, fw3;

struct element {
    unsigned int que[2], orb, orb1, pp, bp, pp1, bp1, list[2], perm, wa;
};

unsigned int nn, rr, nm, nm1, nl, n1, n2, u, pl;
long lstr, llev, bm;
unsigned char *cd, *lv, *matr, *str1, *str2;

static int inpm(r, code, r1, poserr)
    unsigned int r, *r1, *poserr;
    unsigned char *code;
{
    char s;
    unsigned int i, i1, j, rj, x, x0;
    unsigned char sc[RMAX], c[RMAX];

    for (i = 0; i < r; i++) {
	sc[i] = 0;
	c[i] = i;
    }
    for (*r1 = r, rj = 1, x0 = j = 0; (s = nxtsym()); j++)
	switch (rj) {
	case 1:
	    if (s == '(') {
		rj = 2;
		i = 0;
		break;
	    }
	    *poserr = j;
	    return 0;
	case 2:
	    if (s >= '0' && s <= '9' && (x = s - '0') < r) {
		rj = 3;
		break;
	    }
	    *poserr = j;
	    return 0;
	case 3:
	    switch (s) {
	    case ',':
		if (sc[x]) {
		    prt(3, "%u repeated\n", x);
		    *poserr = j - 1;
		    return 0;
		}
		if (i)
		    c[x] = x0;
		else
		    x0 = x;
		sc[x] = 1;
		i++;
		rj = 2;
		(*r1)--;
		break;
	    case ')':
		if (sc[x]) {
		    prt(3, "%u repeated\n", x);
		    *poserr = j - 1;
		    return 0;
		}
		if (!i) {
		    *poserr = j;
		    return 0;
		}
		c[x] = x0;
		sc[x] = 1;
		rj = 1;
		break;
	    default:
		if (s < '0' || s > '9' || (x = 10 * x + (s - '0')) >= r) {
		    *poserr = j;
		    return 0;
		}
	    }
	}
    if (rj != 1) {
	*poserr = j - 1;
	return 0;
    }
    for (i1 = i = 0; i < r; i++)
	if (c[i] == i) {
	    for (j = 0; j < r; j++)
		if (c[j] == i)
		    code[j] = i1;
	    i1++;
	}
    return 1;
}

static int inpmerg(r, code, r1)
    unsigned int r, *r1;
    unsigned char *code;
{
    unsigned int poserr;

    while (1) {
	if (flis)
	    prt(3, "merging (give . to quit):\n");
	readstr(fis, flis);
        if(strequal(".\n"))
	    return 0;
	if (inpm(r, code, r1, &poserr))
	    break;
	prt(3, "error in position %d\n", poserr);
	if (!(flis & KBDBIT))
	    longjmp(jb, 1);
    }
    return 1;
}

static void find(n, r, code, ns, na, nb, base, baut)
    unsigned int n, r, ns, *na, *nb, *base;
    unsigned char *code;
    long *baut;
{
    unsigned int i, j1, j2, na1, nb1, base1[NBMAX];
    unsigned char c[RMAX];
    long beg, b, l;
    double o, om;

    lseek(fw2, 0l, SEEK_SET);
    for (om = 1.0, b = *baut = *na = *nb = i = 0; i < ns; i++) {
	read0(fw2, (char *) c, (int) r * sizeof(*c));
	read0(fw2, (char *) &o, sizeof(o));
	read0(fw2, (char *) &nb1, sizeof(nb1));
	read0(fw2, (char *) base1, (int) nb1 * sizeof(*base));
	read0(fw2, (char *) &na1, sizeof(na1));
	lseek(fw2, l = ((long) n) * na1 * sizeof(int), SEEK_CUR);
	if (o > om) {
	    for (j1 = 1; j1 < r; j1++) {
		for (j2 = 0; j2 < j1; j2++)
		    if (c[j1] == c[j2] && code[j1] != code[j2])
			break;
		if (j2 != j1)
		    break;
	    }
	    if (j1 == r) {
	      om = o;
	      beg = b;
	      *na = na1;
	      *nb = nb1;
	      bcopy((char *) base1, (char *) base, (int) nb1 * sizeof(*base));
	    }
	}
	b += l + r * sizeof(*c) + sizeof(o) + sizeof(nb1) +
	  nb1 * sizeof(*base) + sizeof(na1);
    }
    if (*na)
	*baut = beg + r * sizeof(*c) + sizeof(o) + sizeof(*nb) +
	  (*nb) * sizeof(*base) + sizeof(*na);
}

static int newpart(r, up, v, npart, nlist, eq, a, str)
    unsigned char up, *str[];
    unsigned int r, *v, *npart, *nlist, *eq;
    struct element *a;
{
    unsigned int i, i1, j, k, iw, bw, ew, x, p, s, *t, *t1, n0, n1, m0,
     m1;
    unsigned int cnt[RMAX], cnt1[RMAX];
    struct element *b, *b1, *e, *y;

    for (bw = i1 = i = 0; i < *npart; i++, bw = ew)
	if (r > 3)
	    if ((p = a[i].pp) > 1) {
		for (k = 0; k < r; k++)
		    cnt[k] = cnt1[k] = 0;
		for (e = (b1 = b = a + a[i].bp) + p; b1 < e; b1++)
		    if ((x = *(b1->list)) != *v)
			cnt[*(*str + x)]++;
		if (up)
		    for (b1 = b; b1 < e; b1++)
			if ((x = *(b1->list + 1)) != *(v + 1)) {
			    s = (++cnt1[k = *(*(str + 1) + x)]);
			    if (s > cnt[k])
				return 0;
			}
		for (j = 0; j <= up; j++) {
		    for (ew = bw, s = k = 0; k < r; k++) {
			cnt1[k] = ew;
			ew += (x = cnt[k]);
			if (x == p)
			    s = 1;
		    }
		    if (!s) {
			for (b1 = b; b1 < e; b1++)
			    if ((x = *(b1->list + j)) != *(v + j))
				(a + (cnt1[*(*(str + j) + x)]++))->wa = x;
			for (iw = bw; iw < ew; iw++) {
			    *(t = (a + iw)->list + j) = x = (a + iw)->wa;
			    if (!up)
				*(t + 1) = x;
			}
		    } else
			for (iw = bw, b1 = b; b1 < e; b1++)
			    if ((x = *(b1->list + j)) != *(v + j)) {
				*(t = (a + (iw++))->list + j) = x;
				if (!up)
				    *(t + 1) = x;
			    }
		}
		for (ew = bw, k = 0; k < r; k++)
		    if ((x = cnt[k]) > 0) {
			(y = a + (i1++))->bp1 = ew;
			y->pp1 = x;
			if (x == 1) {
			  *(t1 = (a + ((*eq)++))->que) = *(t = (a + ew)->list);
			  *(t1 + 1) = *(t + 1);
			}
			ew += x;
		    }
	    } else if ((x = *(t = (a + a[i].bp)->list)) != *v) {
		if (up && (*(*str + x) != *(*(str + 1) + *(t + 1))))
		    return 0;
		*(t1 = (a + bw)->list) = x;
		*(t1 + 1) = *(t + 1);
		ew = ((y = a + (i1++))->bp1 = bw) + 1;
		y->pp1 = 1;
	    } else
		ew = bw;
	else if ((p = a[i].pp) > 1) {
	    for (ew = bw, e = (b1 = b = a + a[i].bp) + p, n0 = n1 = 0;
		 b1 < e; b1++)
		if ((x = *(b1->list)) != *v) {
		    if (*(*str + (x >> 3)) & (128 >> (x & 7)))
			n1++;
		    else
			n0++;
		}
	    if (up)
		for (b1 = b, m0 = m1 = 0; b1 < e; b1++)
		    if ((x = *(b1->list + 1)) != *(v + 1)) {
			if (*(*(str + 1) + (x >> 3)) & (128 >> (x & 7))) {
			    if (m1 == n1)
				return 0;
			    m1++;
			} else {
			    if (m0 == n0)
				return 0;
			    m0++;
			}
		    }
	    if (n0 + n1) {
		for (j = 0; j <= up; j++)
		    if (n0 && n1) {
			for (ew = (iw = bw) + n0, b1 = b; b1 < e; b1++)
			    if ((x = *(b1->list + j)) != *(v + j)) {
			      if (*(*(str + j) + (x >> 3)) & (128 >> (x & 7)))
				(a + (ew++))->wa = x;
			      else
				(a + (iw++))->wa = x;
			    }
			for (ew = (iw = bw) + n0 + n1; iw < ew; iw++) {
			    *(t = (a + iw)->list + j) = x = (a + iw)->wa;
			    if (!up)
				*(t + 1) = x;
			}
		    } else
			for (ew = (iw = bw) + n0 + n1, b1 = b; b1 < e; b1++)
			    if ((x = *(b1->list + j)) != *(v + j)) {
				*(t = (a + (iw++))->list + j) = x;
				if (!up)
				    *(t + 1) = x;
			    }
		(a + i1)->bp1 = bw;
		(a + (i1++))->pp1 = x = (n0) ? n0 : n1;
		if (x == 1) {
		    *(t1 = (a + ((*eq)++))->que) = *(t = (a + bw)->list);
		    *(t1 + 1) = *(t + 1);
		}
		if (n0 && n1) {
		    (a + i1)->bp1 = bw + n0;
		    (a + (i1++))->pp1 = n1;
		    if (n1 == 1) {
		      *(t1 = (a + ((*eq)++))->que) =
			*(t = (a + (bw + n0))->list);
		      *(t1 + 1) = *(t + 1);
		    }
		}
	    }
	} else if ((x = *(t = (a + a[i].bp)->list)) != *v) {
	    s = *(t + 1);
	    if (up &&
		(((*(*str + (x >> 3)) & (128 >> (x & 7)))) ? 1 : 0) !=
		(((*(*(str + 1) + (s >> 3)) & (128 >> (s & 7)))) ? 1 : 0))
		return 0;
	    *(t1 = (a + bw)->list) = x;
	    *(t1 + 1) = *(t + 1);
	    ew = ((y = a + (i1++))->bp1 = bw) + 1;
	    y->pp1 = 1;
	} else
	    ew = bw;
    for (*nlist = ew, e = (b = a) + (*npart = i1); b < e; b++) {
	b->pp = b->pp1;
	b->bp = b->bp1;
    }
    return (1);
}

static void getgr(x, astr)
    unsigned int x;
    unsigned char **astr;
{
    unsigned char *str, *b, *e, buf[1000];
    unsigned int i, z, y, v;

    if (x < nm)
	*astr = (unsigned char *) (matr + lstr * x);
    else if (x == n1) {
	*astr = str1;
	u = 1;
    } else if (x == n2) {
	*astr = str2;
	u = 0;
    } else {
	*astr = (str = (u) ? str2 : str1);
	if (u)
	    n2 = x;
	else
	    n1 = x;
	u = 1 - u;
	lseek(fic, bm + (long) nn * x, SEEK_SET);
	if (rr > 3) {
	    read0(fic, (char *) str, (int) lstr);
	    for (e = str + nn; str < e; str++)
		*str = *(cd + *str);
	} else
	    for (z = nn; z; z -= y) {
		v = ((y = (z > 1000) ? 1000 : z) + 7) >> 3;
		read0(fic, (char *) buf, (int) y);
		for (b = buf, e = str + v; str < e; str++)
		    for (*str = i = 0; i < 8; i++, b++)
			if (*(cd + *b) == 1)
			    *str |= (128 >> i);
	    }
    }
}

static int testaut(r, bq, n, a)
    unsigned int r, bq, n;
    struct element *a;
{
    unsigned int i, j, *x, y1, y2;
    unsigned char *s1, *s2;

    for (i = bq; i < n; i++) {
	getgr(y1 = *(x = a[i].que), &s1);
	getgr(y2 = *(x + 1), &s2);
	a[y1].perm = y2;
	if (r > 3) {
	    for (j = bq; j < n; j++) {
	        x = a[j].que;
		if (*(s1 + *x) != *(s2 + *(x + 1)))
		    return 0;
	    }
	} else
	    for (j = bq; j < n; j++) {
		y1 = *(x = a[j].que);
		y2 = *(x + 1);
		if (((*(s1 + (y1 >> 3)) & (128 >> (y1 & 7))) ? 1 : 0) !=
		    ((*(s2 + (y2 >> 3)) & (128 >> (y2 & 7))) ? 1 : 0))
		    return 0;
	    }
    }
    return (1);
}

static void clorb(n, a)
    unsigned int n;
    struct element *a;
{
    unsigned int i, i1, j1, j2;

    for (i = 0; i < n; i++) {
	if ((j1 = a[a[i].perm].orb) != (j2 = a[i].orb)) {
	    if (j1 < j2) {
		j2 = j1;
		j1 = a[i].orb;
	    }
	    for (i1 = 0; i1 < n; i1++)
		if (a[i1].orb == j1)
		    a[i1].orb = j2;
	}
    }
}

static void mult(s, ord)
    unsigned int s, *ord;
{
    int i, x;
    long sl, w;

    for (sl = s, x = 0, i = NORD - 1; i >= 0; i--) {
	ord[i] = (w = sl * ord[i] + x) % 10000;
	x = w / 10000;
    }
}

struct level {
    unsigned int lev, np, nl, bq, fp;
    struct part {
	unsigned int bp, pp, l1, l2;
    } part[1];
};

static void startset(n, r, beg, code)
    unsigned int n, r, beg;
    unsigned char *code;
{
    unsigned int i, x, y, v;
    unsigned char *b, *b1, *e, *e1, str[1000];
/*  long z, zm, zl; */

    nn = n;
    rr = r;
    bm = beg;
    cd = code;
    lstr = n * sizeof(*matr);
    if (r <= 3)
	lstr = (lstr + 7) >> 3;
    llev = sizeof(struct level) + ((long) n - 1) * sizeof(struct part);
/*
    zm = n * lstr;
    zl = NBMAX * llev;
    if (zm + llev <= (z = (long) (allocmem(60000, NULL) - 2) * 16)) {
	nm = nm1 = n;
	nl = (zm + zl <= z) ? NBMAX : (z - zm) / llev;
	pl = 0;
    } else if (2 * lstr + llev <= z) {
	nm = (nm1 = (z - llev) / lstr) - 2;
	n1 = n2 = n;
	u = 0;
	nl = 1;
    } else {
	prt(3, "not enough space\n");
	longjmp(jb, 1);
    }
*/
    nm = nm1 = n;
    nl = NBMAX;
    pl = 0;

    if((matr = (unsigned char *) getmem(nm1, lstr)) == NULL) {
      prt(3, "Out of memory\n");
      longjmp(jb, 1);
    }

    if (nm < n)
	str2 = ((str1 = (matr + nm * lstr)) + lstr);

    x = (llev + 15) / 16;
    lv = (unsigned char *)
      getmem(x, (unsigned int) ((llev * nl + (x - 1)) / x));
    if(lv == NULL) {
      prt(3, "Out of memory\n");
      longjmp(jb, 1);
    }
    lseek(fic, bm, SEEK_SET);
    for (e = (b = matr) + nm * lstr; b < e; ) {
	if (r > 3) {
	    read0(fic, (char *) b, (int) lstr);
	    for (e1 = b + lstr; b < e1; b++)
		*b = *(code + *b);
	} else
	    for (x = n; x; x -= y) {
		v = ((y = (x > 1000) ? 1000 : x) + 7) >> 3;
		read0(fic, (char *) str, (int) y);
		for (b1 = str, e1 = b + v; b < e1; b++)
		    for (*b = i = 0; i < 8; i++, b1++)
			if (*(code + *b1) == 1)
			    *b |= (128 >> i);
	    }
    }
}

static void freematr()
{
    free(matr);
    free(lv);
}

static void lwrite(f, p, s)
    int f;
    char *p;
    long s;
{
    unsigned int s1;

    for (; s; s -= s1, p += s1)
      if (write(f, (void *) p, (s1 = (s > 60000l) ? 60000 : s)) <= 0) {
	prt(3, "output error on work file aut#3.tmp : %s\n", strerror(errno));
	longjmp(jb, 1);
      }
}

static void lread(f, p, s)
    int f;
    char *p;
    long s;
{
    unsigned int s1;

    for (; s; s -= s1, p += s1)
	read0(f, (char *) p, (int) (s1 = (s > 60000l) ? 60000 : s));
}

static unsigned int laut(baut, base)
    long baut;
    unsigned int *base;
{
    unsigned int i, x, y;

    for (i = 0;; i++) {
	lseek(fw2, baut + (x = base[i]) * sizeof(y), SEEK_SET);
	read0(fw2, (char *) &y, sizeof(y));
	if (y != x)
	    break;
    }
    return (i + 1);
}

static void putlev(lev, npart, nlist, bq, fp, a)
    unsigned int lev, npart, nlist, bq, fp;
    struct element *a;
{
    unsigned int *y;
    struct part *b, *e;
    struct level *x;
    struct element *b1;

    if (lev > NBMAX) {
	prt(3, "level is greater than %u\n", NBMAX);
	longjmp(jb, 1);
    }
    if (nl == NBMAX)
	x = (struct level *) (lv + llev * (lev - 1));
    else {
	x = (struct level *) (lv + llev * (pl++));
	if (pl == nl)
	    pl = 0;
    }
    for (x->lev = lev, x->bq = bq, x->fp = fp, b1 = a,
	 e = (b = x->part) + (x->np = npart);
	 b < e; b++, b1++) {
	b->pp = b1->pp;
	b->bp = b1->bp;
    }
    for (b1 = a, e = (b = x->part) + (x->nl = nlist); b < e; b++, b1++) {
	b->l1 = *(y = b1->list);
	b->l2 = *(y + 1);
    }
    if (nl < NBMAX) {
	lseek(fw3, llev * (lev - 1), SEEK_SET);
	lwrite(fw3, x, llev);
    }
}

static void getlev(lev, npart, nlist, bq, fp, a)
    unsigned int lev, *npart, *nlist, *bq, *fp;
    struct element *a;
{
    unsigned int *y;
    struct part *b, *e;
    struct level *x;
    struct element *b1;

    if (nl == NBMAX)
	x = (struct level *) (lv + llev * (lev - 1));
    else {
	if (pl)
	    pl--;
	else
	    pl = nl - 1;
	x = (struct level *) (lv + llev * (pl++));
	if (pl == nl)
	    pl = 0;
	if (x->lev != lev) {
	    lseek(fw3, llev * (lev - 1), SEEK_SET);
	    lread(fw3, x, llev);
	}
    }
    for (*bq = x->bq, *fp = x->fp, b1 = a,
	 e = (b = x->part) + (*npart = x->np);
	 b < e; b++, b1++) {
	b1->pp = b->pp;
	b1->bp = b->bp;
    }
    for (b1 = a, e = (b = x->part) + (*nlist = x->nl); b < e; b++, b1++) {
	*(y = b1->list) = b->l1;
	*(y + 1) = b->l2;
    }
}

static void readaut(beg, n, a)
    long beg;
    unsigned int n;
    struct element *a;
{
    unsigned int i, k, l, perm[1000];
    struct element *pa;

    lseek(fw2, beg, SEEK_SET);
    for (pa = a, k = n; k; k -= l) {
	read0(fw2, (char *) perm,
	      (int)(l = (k < 1000) ? k : 1000) * sizeof(*perm));
	for (i = 0; i < l; i++, pa++)
	    pa->perm = perm[i];
    }
}

static void writaut(n, a)
    unsigned int n;
    struct element *a;
{
    unsigned int i, k, l, perm[1000];
    struct element *pa;

    for (pa = a, k = n; k; k -= l) {
	for (l = (k < 1000) ? k : 1000, i = 0; i < l; i++, pa++)
	    perm[i] = pa->perm;
	if (write(fw1, perm, l * sizeof(*perm)) <= 0) {
	    prt(3, "output error on work file aut#1.tmp : %s\n",
		strerror(errno));
	    longjmp(jb, 1);
	}
    }
}

static void xwrite(int fd, const void *buf, size_t count) {
    ssize_t res = write(fd, buf, count);

    if (res == -1) {
	prt(3, "output error on work file aut#2.tmp : %s\n",
	    strerror(errno));
	longjmp(jb, 1);
    }
}

static void outaut(n, r, code, ord, na, nb, base, end)
    unsigned int n, r, *ord, na, nb, *base;
    unsigned char *code;
    long *end;
{
    unsigned int i, k, l, perm[1000];
    double o;

    lseek(fw2, *end, SEEK_SET);
    xwrite(fw2, code, r * sizeof(*code));
    for (o = i = 0; i < NORD; i++)
	o = o * 10000.0 + ord[i];
    xwrite(fw2, &o, sizeof(o));
    xwrite(fw2, &nb, sizeof(nb));
    xwrite(fw2, base, nb * sizeof(*base));
    xwrite(fw2, &na, sizeof(na));
    for (lseek(fw1, 0l, SEEK_SET), i = 0; i < na; i++)
	for (k = n; k; k -= l) {
	    read0(fw1, (char *) perm,
		  sizeof(*perm) * (int) (l = (k < 1000) ? k : 1000));
	    xwrite(fw2, perm, sizeof(*perm) * l);
	}
    (*end) += ((long) n) * na * sizeof(*perm) + r * sizeof(*code) +
      sizeof(o) + sizeof(nb) + nb * sizeof(*base) + sizeof(na);
}

static void prord(ord)
    unsigned int *ord;
{
    unsigned int i, x;

    for (i = 0; i < NORD; i++)
	if (ord[i])
	    break;
    prt(3, "%u", ord[i++]);
    for (; i < NORD; i++)
	for (x = 1000; x; x /= 10)
	    if (ord[i] >= x) {
		prt(3, "%u", ord[i]);
		break;
	    } else
		prt(3, "0");
}

static void outlist(str, b, deg)
    char *str;
    unsigned int b, *deg;
{
    unsigned i, ls, lb;
    char s;

    for (ls = strlen(str), i = 0; i < b; i++) {
	if (!i) {
	    Sprintf(str + ls, "%u", *deg);
	    lb = ls;
	} else {
	    Sprintf(str + ls, ",%u", deg[i]);
	    lb = ls + 1;
	}
	if ((ls = strlen(str)) > 75) {
	    ls -= lb;
	    s = str[lb];
	    str[lb] = '\0';
	    prt(3, "%s*\n", str);
	    *str = s;
	    bcopy(str + lb + 1, str + 1, ls);
	}
    }
    prt(3, "%s\n", str);
}

static void aut(n, r, beg, code, nb, base, na, baut, ord, a)
    unsigned int n, r, beg, *nb, *base, *na, *ord;
    unsigned char *code;
    long baut;
    struct element *a;
{
    unsigned int i, i0, im, j, lev, fp, min, max, *wp, v[2], s, laut();
    unsigned int nlist, npart, bq, eq, fix[NBMAX], f, fm, fx, levaut, ia,
     na1;
    unsigned char up, dw, *str[2];
    struct element *pa;

    startset(n, r, beg, code);
    for (pa = a, a->pp = nlist = n, npart = 1, a->bp = i = 0; i < n; i++, pa++)
	*(pa->list) = *((pa->list) + 1) = pa->orb1 = pa->orb = i;
    for (i = 0; i < NORD - 1; i++)
	ord[i] = 0;
    if (*na)
	levaut = laut(baut, base);
    for (na1 = *na, *na = ia = fx = up = bq = eq = 0,
	 ord[NORD - 1] = lev = 1; lev > 0; ) {
	dw = 0;
	if (eq == n)
	    if (up)
		if (testaut(r, bq, n, a)) {
		    writaut(n, a);
		    (*na)++;
		    clorb(n, a);
		    lev = fx;
		} else
		    lev--;
	    else {
		up = 1;
		fx = lev = lev - 1;
	    }
	else {
	    if (bq == eq) {
		if (!up && lev <= *nb)
		    for (pa = a, fp = 0; fp < npart; fp++, pa++) {
		      for (im = (i0 = i = pa->bp) + pa->pp; i < im; i++)
			if (*(a[i].list) == base[lev - 1])
			  break;
		      if (i < im) {
			for (; i > i0; i--)
			  *(a[i].list) = *(a[i].list + 1) = *(a[i - 1].list);
			*(a[i0].list) = *(a[i0].list + 1) = base[lev - 1];
			break;
		      }
		    }
		else {
		    if (lev <= 2) {
			for (pa = a, max = i = 0; i < npart; i++, pa++)
			    if ((pa->pp) > max) {
				fp = i;
				max = pa->pp;
			    }
		    } else
			for (pa = a, min = n + 1, i = 0; i < npart; i++, pa++)
			    if ((pa->pp) > 1 && (pa->pp) < min) {
				fp = i;
				min = pa->pp;
			    }
		    if (!up) {
			base[lev - 1] = *(a[a[fp].bp].list);
			*nb = lev;
		    }
		}
		fix[lev - 1] = 0;
		putlev(lev, npart, nlist, bq, fp, a);
		*(a[eq].que) = *(wp = a[a[fp].bp].list);
		*(a[eq++].que + 1) = *(wp + 1);
		lev++;
	    }
	    v[0] = a[bq].que[0];
	    a[v[0]].perm = v[1] = a[bq].que[1];
	    getgr(v[0], str);
	    if (up)
		getgr(v[1], &str[1]);
	    if (newpart(r, up, v, &npart, &nlist, &eq, a, str)) {
		dw = 1;
		bq++;
	    } else
		lev--;
	}
	if (!dw)
	    for (; lev; lev--) {
		getlev(lev, &npart, &nlist, &bq, &fp, a);
		eq = bq;
		for (; ia < na1 && lev == levaut; ia++, (*na)++) {
		    readaut(baut, n, a);
		    writaut(n, a);
		    clorb(n, a);
		    if (ia < na1 - 1)
			levaut = laut((baut += n * sizeof(int)), base);
		}
		for (fm = a[fp].pp, f = fix[lev - 1] + 1; f < fm; f++) {
		    i = *(a[a[fp].bp + f].list + 1);
		    if (lev > fx || a[i].orb == i)
			break;
		}
		if (f < fm) {
		    *(a[eq++].que + 1) = i;
		    fix[(lev++) - 1] = f;
		    break;
		}
		if (lev == fx) {
		    if (lev == 2)
			for (i = 0; i < n; i++)
			    a[i].orb1 = a[i].orb;
		    for (pa = a, j = a[base[lev - 1]].orb, s = i = 0;
			 i < n; i++, pa++)
			if (pa->orb == j)
			    s++;
		    if (s > 1)
			mult(s, ord);
		    else
			for ((*nb)--, i = lev - 1; i < *nb; i++)
			    base[i] = base[i + 1];
		    fx--;
		}
	    }
    }
    freematr();
}

static void auts(n, r, beg)
    unsigned int n, r, beg;
{
    unsigned char code[RMAX];
    unsigned int i, j, b, s, r1, ns, nb, base[NBMAX], na, ord[NORD], deg[RMAX];
    long baut, end;
    char str[85];
    struct element *a;

    if ((a = Getmem(n, struct element)) == NULL) {
	prt(3, "not enough space\n");
	longjmp(jb, 1);
    }
    for (end = ns = 0; inpmerg(r, code, &r1); ns++) {
	prt(3, "colour graph of rank %u\n", r1);
	find(n, r, code, ns, &na, &nb, base, &baut);
	lseek(fw1, 0l, SEEK_SET);
	aut(n, r1, beg, code, &nb, base, &na, baut, ord, a);
	for (b = i = 0; i < n; i++)
	    if (a[i].orb == i) {
		for (s = j = 0; j < n; j++)
		    if (a[j].orb == i)
			s++;
		deg[b++] = s++;
	    }
	if (b == 1)
	    prt(3, "transitive");
	else
	    prt(3, "intransitive");
	prt(3, " automorphism group of order ");
	prord(ord);
	prt(3, "\n");
	if (b == 1) {
	    for (b = i = 0; i < n; i++)
		if (a[i].orb1 == i) {
		    for (s = j = 0; j < n; j++)
			if (a[j].orb1 == i)
			    s++;
		    deg[b++] = s;
		}
	    Sprintf(str, "rank=%u; subdegrees:", b);
	} else
	    Sprintf(str, "with %u orbits of lengths ", b);
	outlist(str, b, deg);
	prt(3, "base of length %u, %u generators\n\n", nb, na);
	outaut(n, r, code, ord, na, nb, base, &end);
    }
    free((char *) a);
}

static void openf(na, pa, n, r, beg)
    int na;
    char *pa[];
    unsigned int *n, *r, *beg;
{
    unsigned char mode, matr[NOMAX * NOMAX];
    unsigned int b, i, lorb[NOMAX];
    char buf[1000];
    char str[85];

    if (na <= 2) {
	prt(3, "no input file for color graph\n");
	longjmp(jb, 1);
    }
    Sprintf(buf, "%s.cgr", pa[2]);
    if ((fic = open(buf, O_RDONLY)) < 0) {
	if ((fic = open(pa[2], O_RDONLY)) < 0) {
	    prt(3, "open error on file %s : %s\n", pa[2], strerror(errno));
	    longjmp(jb, 1);
	}
        Strcpy(buf, pa[2]);
    }
    prt(3, "input of colour graph from file %s\n", buf);

    if(!openfile("input of mergings from %s\n",
		 &inmergf, (na > 3) ? pa[3] : "*", "sub", "rt"))
      longjmp(jb, 1);

    prt(3, "\n");
    lseek(fic, 0l, SEEK_SET);
    read0(fic, (char *) &mode, sizeof(mode));
    read0(fic, (char *) &b, sizeof(b));
    if (b == 0 || b > NOMAX) {
	prt(3, "incorrect number of blocks\n");
	longjmp(jb, 1);
    }
    read0(fic, (char *) lorb, (int) b * sizeof(*lorb));
    for (*n = i = 0; i < b; i++) {
	if (lorb[i] == 0) {
	    prt(3, "zero block size?\n");
	    longjmp(jb, 1);
	}
        *n += lorb[i];
    }
    read0(fic, (char *) matr, (int)(b * b * sizeof(*matr)));
    for (*r = i = 0; i < b * b; i++)
	if (matr[i] == 0 || matr[i] > RMAX || (*r += matr[i]) > RMAX) {
	    prt(3, "incorrect rank\n");
	    longjmp(jb, 1);
	}
    *beg = sizeof(mode) + sizeof(b) + b * sizeof(*lorb) +
      b * b * sizeof(*matr) + (*r) * sizeof(int);
    if (b == 1)
	prt(3, "homogeneous ");
    prt(3, "colour graph of rank %u on %u vertices\n", *r, *n);
    if (b > 1) {
	Sprintf(str, "%u blocks of size ", b);
	outlist(str, b, lorb);
    }
    prt(3, "\n");
    if ((fw1 = open("aut#1.tmp", O_RDWR | O_CREAT, 0666)) < 0) {
	prt(3, "open error on work file aut#1.tmp : %s\n", strerror(errno));
	longjmp(jb, 1);
    }
    if ((fw2 = open("aut#2.tmp", O_RDWR | O_CREAT, 0666)) < 0) {
	prt(3, "open error on work file aut#2.tmp : %s\n", strerror(errno));
	longjmp(jb, 1);
    }
    if ((fw3 = open("aut#3.tmp", O_RDWR | O_CREAT, 0666)) < 0) {
	prt(3, "open error on work file aut#3.tmp : %s\n", strerror(errno));
	longjmp(jb, 1);
    }
}

void cleanup()
{
    close(fic);
    close(fw1);
    unlink("aut#1.tmp");
    close(fw2);
    unlink("aut#2.tmp");
    close(fw3);
    unlink("aut#3.tmp");
}

void Main(na, pa)
    int na;
    char **pa;
{
    unsigned int n, r, beg;

    prt(3, "\n* calculation of automorphism groups *\n\n");
    openf(na, pa, &n, &r, &beg);
    auts(n, r, beg);
}
