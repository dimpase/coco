/*
 * COCO - Unix version 1.0 - aeb - 920530
 *
 * cgr.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include "def.h"
#include "prt.h"
#include "ingp.h"
#include "common.h"
#include "readstr.h"

#define NOMAX 15
#define NRMAX 255

jmp_buf jb;
struct file ingpf, inoptf;
#define fig ingpf.fd
#define fio inoptf.fd
#define flig ingpf.flags
#define flio inoptf.flags
int foc, floc;

struct point {
    unsigned int orb, seq, lorb;
};

struct orbit {
    unsigned int num, rep, beg, length, rank, rank1;
    char sc;
};

struct que {
    char sc;
    unsigned int el;
};

char *virt;
int f;
unsigned int nr, nvirt, lvirt, nc, *cd;

static void getvirt(nrec, lrec)
    unsigned int nrec, lrec;
{
    unsigned int i;

    nr = nrec;
    if ((cd = Getmem(nr, unsigned int)) == NULL) {
	prt(3, "not enough virtual space\n");
	longjmp(jb, 1);
    }
    for (i = 0; i < nr; i++)
	cd[i] = nr;
    *((int *) (&virt)) = nc = 0;
    lvirt = lrec;
    nvirt = nrec;
    while(nvirt && (virt = (char *) getmem(nvirt, lrec)) == NULL)
        nvirt /= 2;
    /* the first nvirt records are in memory, the rest on file */
}

static void readvirt(p, rec)
    void *p;
    unsigned int rec;
{
    unsigned int r;

    if ((r = cd[rec]) < nvirt)
	bcopy(virt + ((long) lvirt) * r, p, lvirt);
    else {
	lseek(f, ((long) lvirt) * (r - nvirt), SEEK_SET);
	read0(f, p, lvirt);
    }
}

static void frevirt()
{

    free(cd);
    free(virt);
    if (nc > nvirt) {
	close(f);
	unlink("cgr#2.tmp");
    }
}

static void writvirt(p, rec)
    void *p;
    unsigned int rec;
{
    unsigned int r;

    if ((r = cd[rec]) == nr) {
	if ((r = nc) == nvirt)
	    if ((f = open("cgr#2.tmp", O_RDWR | O_CREAT, 0666)) < 0) {
		prt(3, "open error on work file cgr#2.tmp : %s\n",
		    strerror(errno));
		longjmp(jb, 1);
	    }
	cd[rec] = nc++;
    }
    if (r < nvirt)
	bcopy(p, virt + ((long) lvirt) * r, lvirt);
    else {
	lseek(f, ((long) lvirt) * (r - nvirt), SEEK_SET);
	if (write(f, p, lvirt) <= 0) {
	    prt(3, "output error on work file cgr#2.tmp : %s\n",
		strerror(errno));
	    frevirt();
	    longjmp(jb, 1);
	}
    }
}

static void orbits(n, ng, gens, no, lorb)
    unsigned int n, ng, *gens, *no, lorb[];
{
    unsigned int s, l, max, *pg, *eg, x;
    struct que *que, *pq, *eq;
    long d;
    register int i;

    if ((que = Getmem(n, struct que)) == NULL) {
	prt(3, "not enough space for construction of orbits\n");
	longjmp(jb, 1);
    }
    for (i = 0; i < n; i++)
        que[i].sc = 1;
    for (d = ng, *no = s = 0; s < n; s += l, (*no)++) {
	if (*no == NOMAX) {
	    prt(3, "number of orbits is greater than %d\n", NOMAX);
	    longjmp(jb, 1);
	}
	for (eq = (pq = que) + 1, max = pq->el = s, (pq + s)->sc = l = 0;
	     pq < eq; pq++, l++)
	    for (eg = (pg = gens) + ng; pg < eg; pg++)
		if ((que + (x = *(pg + d * (pq->el))))->sc) {
		    (eq++)->el = x;
		    (que + x)->sc = 0;
		    if (max < x)
			max = x;
		}
	if (max >= s + l) {
	    prt(3, "wrong order of elements of orbit %u\n", *no + 1);
	    longjmp(jb, 1);
	}
	prt(3, "orbit %u has length %u\n", *no + 1, (lorb[*no] = l));
    }
    prt(3, "\n");
    free(que);
}

static void fixpoint(ng, gens, l, r, s, points, ns, orb)
    unsigned int ng, *gens, l, r, s, ns;
    struct point *points;
    struct orbit orb[];
{
    unsigned int i, im, x, x1, x2, x3, lstr, ll, rr;
    unsigned int *csr, *csr1, *csr2, *inv;
    unsigned int *pc, *pc1, *pec, *pg, *peg;
    struct point *pp, *pp1, *pp2, *pp3;
    struct que *que, *pq, *pq1, *peq;
    struct orbit *o, *eo;
    long d;

    lstr = sizeof(*csr) * s;
    if ((csr = Getmem(4 * lstr, unsigned int)) == NULL ||
	(que = Getmem(l, struct que)) == NULL) {
	prt(3, "not enough space\n");
	longjmp(jb, 1);
    }
    inv = (csr2 = (csr1 = csr + s) + s) + s;
    getvirt(l, lstr);
    for (pp = points, pc = csr, eo = (o = orb) + ns; o < eo; o++)
	for (im = (i = o->rep) + (o->length); i < im; i++) {
	    *(pc++) = pp->orb = i;
	    pp->seq = 0;
	    (pp++)->lorb = 1;
	}
    for (pq = que, rr = i = 0; i < l; i++, pq++)
	pq->sc = 1;
    writvirt(csr, 0);
    for (d = ng, que->sc = 0, que->el = r, peq = (pq = que) + 1;
	 pq < peq && rr < ns; pq++) {
	readvirt(csr, (x = pq->el) - r);
	for (peg = (pg = gens) + ng; pg < peg; pg++) {
	    for (pc = csr, pc1 = csr1; pc < csr1; pc++, pc1++)
		*pc1 = *(pg + d * (*pc));
	    if ((pq1 = que + ((x1 = *(pg + d * x)) - r))->sc) {
		pq1->sc = 0;
		(peq++)->el = x1;
		writvirt(csr1, x1 - r);
	    } else {
		readvirt(csr2, x1 - r);
		for (pc = csr2, o = orb; o < eo; o++)
		    for (pc1 = inv - ((i = o->rep) - (o->beg)),
			 im = i + (o->length); i < im; i++)
			*(pc1 + *(pc++)) = i;
		for (pc = csr1, o = orb; o < eo; o++)
		    for (pec = pc + (o->length),
			 pc1 = inv - ((o->rep) - (o->beg)); pc < pec; pc++)
			*pc = *(pc1 + *pc);
		for (pp = points, pc = csr1, o = orb; o < eo; o++) {
		    for (pp1 = points - ((i = o->rep) - (o->beg)),
			 im = i + (o->length);
			 i < im; i++, pp++, pc++)
			if ((x3 = *pc) != i)
			    if ((x1 = pp->orb) != (x2 = (pp1 + x3)->orb)) {
				if (x1 > x2) {
				    x1 = x2;
				    x2 = pp->orb;
				}
				x3 = x2;
				ll = 0;
				(o->rank)--;
				do {
				    (pp2 = pp1 + x2)->orb = x1;
				    x2 = pp2->seq;
				    ll++;
				} while (x2);
				pp2->seq = (pp3 = pp1 + x1)->seq;
				pp3->seq = x3;
				(pp3->lorb) += ll;
			    }
		    if (!(o->sc) && (o->rank) <= (o->rank1)) {
			o->sc = 1;
			rr++;
		    }
		}
	    }
	}
    }
    free(csr);
    free(que);
    frevirt();
}

static void prsubd(mode, i, r, ns, orb, points, k)
    unsigned char mode;
    unsigned int i, r, ns, k;
    struct point *points;
    struct orbit *orb;
{
    char str[100], s;
    unsigned int j, x, x1, xm, lstr, lc;
    struct point *pp, *pp1;
    struct orbit *o;

    prt(3, "stabilizer of point %u of orbit %u has\n", r, i + 1);
    for (pp = points, o = orb, j = 0; j < ns; j++, o++) {
	prt(3, "   %u suborbits on orbit %u\n", o->rank, o->num + 1);
	if (mode != 1) {
	    prt(3, "     number  length   representative\n");
	    for (xm = (x = o->rep) + (o->length); x < xm; x++, pp++)
		if ((pp->orb) == x)
		    prt(3, "%9u%9u%12u\n", k++, pp->lorb, x);
	} else {
	    for (xm = (x = o->rep) + (o->length); x < xm; x++, pp++)
		if ((pp->orb) == x) {
		    Sprintf(str, "    suborbit %u of length %u: %u",
			    k++, pp->lorb, x);
		    lstr = strlen(str);
		    for (pp1 = pp + 1, x1 = x + 1; x1 < xm; x1++, pp1++)
			if ((pp1->orb) == x) {
			    Sprintf(str + lstr, ",%u\0", x1);
			    lc = lstr;
			    if ((lstr = strlen(str)) > 75) {
				s = str[lc + 1];
				str[lc + 1] = '\0';
				prt(3, "%s\n", str);
				Sprintf(str, "     ");
				*(str + 5) = s;
				bcopy(str + lc + 2, str + 6, lstr - lc - 1);
				lstr -= (lc - 4);
			    }
			}
		    prt(3, "%s\n", str);
		}
	}
    }
}

static void writp(f, points, lp)
    int f;
    char *points;
    long lp;
{

    for (; lp > 0; points += 60000, lp -= 60000)
	if (write(f, (char *) points, (lp > 60000) ? 60000 : lp) <= 0) {
	    prt(3, "output error on work file cgr#1.tmp : %s\n",
		strerror(errno));
	    (void) close(f);
	    (void) unlink("cgr#1.tmp");
	    free(points);
	    longjmp(jb, 1);
	}
}

static void readp(f, points, lp)
    int f;
    char *points;
    long lp;
{

    for (; lp > 0; points += 60000, lp -= 60000)
	read0(f, (char *) points, (lp > 60000) ? 60000 : lp);
}

static void xwrite(int fd, const void *buf, size_t count) {
    ssize_t res = write(fd, buf, count);

    if (res == -1) {
	prt(3, "output error on file of colour graph : %s\n",
	    strerror(errno));
	longjmp(jb, 1);
    }
}

static void outhead(mode, no, lorb, matr, k, asym, lhead)
    unsigned int no, *lorb, k, *asym, *lhead;
    unsigned char mode;
    char *matr;
{
    char m;
    unsigned int i;
    int sym[NRMAX];

    m = mode - 2;
    xwrite(foc, &m, sizeof(m));
    xwrite(foc, &no, sizeof(no));
    xwrite(foc, lorb, no * sizeof(*lorb));
    xwrite(foc, matr, no * no * sizeof(*matr));
    for (i = 0; i < k; i++)
	sym[i] = -2;
    xwrite(foc, sym, k * sizeof(*sym));
    *lhead = (*asym = sizeof(m) + sizeof(no) + no * sizeof(*lorb) +
	      no * no * sizeof(*matr)) + k * sizeof(*sym);
}

static void outcgr(ng, gens, r, mode, ns, orb, l, s, points, k, beg)
    unsigned char mode;
    unsigned int ng, *gens, r, ns, l, s, k;
    struct orbit *orb;
    struct point *points;
    long beg;
{
    unsigned char dg, *str, *str1, *ps, *ps1, *g;
    unsigned int b, u, x, x1, xm, i, ls, ls1, q, lev, lo, rep[NRMAX];
    unsigned int *pg, *stk;
    struct point *pp, *pp1;
    struct orbit *o, *eo;
    struct que *que, *pq, *peq;
    struct tree {
	unsigned int b;
	char dg;
    } *tree, *pt;
    long lstr, lstr1, d;

    if ((que = Getmem(l, struct que)) == NULL ||
	(tree = Getmem(l, struct tree)) == NULL ||
	(g = Getmem(l, unsigned char)) == NULL) {
	prt(3, "not enough space\n");
	longjmp(jb, 1);
    }
    for (pt = tree, peq = (pq = que) + l; pq < peq; pq++, pt++) {
	pq->sc = 1;
	pt->dg = 0;
    }
    for (d = ng, que->sc = b = 0, que->el = r, peq = (pq = que) + 1, q = 1;
	 q < l; pq++, pt->dg = dg) {
	for ((pt = tree + ((x = pq->el) - r))->b = b, pg = gens + d * x,
	     dg = i = 0; i < ng; i++, pg++)
	    if ((que + (x1 = *pg - r))->sc) {
		(que + x1)->sc = 0;
		(peq++)->el = *pg;
		*(g + (b++)) = i;
		dg++;
		q++;
	    }
    }
    free(que);
    ls = s;
    if ((str = Getmem(2*ls, unsigned char)) == NULL ||
	(stk = Getmem(l, unsigned int)) == NULL) {
	prt(3, "not enough space\n");
	longjmp(jb, 1);
    }
    for (ls1 = i = 0, pp = points, eo = (o = orb) + ns; o < eo; o++)
	for (ps = (unsigned char *) (str - ((x = o->rep) - (o->beg))),
	     xm = x + (o->length);
	     x < xm; x++, pp++, i++)
	    if ((pp->orb) == x) {
		if (k == NRMAX) {
		    prt(3, "number of 2-orbits is greater than %d\n", NRMAX);
		    longjmp(jb, 1);
		}
		for (pp1 = pp, x1 = x; x1 < xm; x1++, pp1++)
		    if ((pp1->orb) == x)
			*(ps + x1) = k;
		k++;
		rep[ls1++] = i;
	    }
    if (mode == 3)
	ls1 = ls;
    lstr1 = ls1;
    lseek(foc, beg, SEEK_SET);
    xwrite(foc, (char *) str, ls);
    for (i = 1; i < l; i++)
	    xwrite(foc, (char *) str, ls1);
    str1 = str + ls;
    getvirt(l, ls);
    if (tree->dg > 1)
	writvirt(str, 0);
    for (lstr = ls, *stk = lo = lev = 0, q = 1; q < l; q++) {
	if (lev != lo) {
	    readvirt(str, lev);
	    lo = lev;
	}
	for (pg = gens + *(g + ((pt = tree + (u = *(stk + lev)))->b)++),
	     ps = (unsigned char *) str, o = orb; o < eo; o++)
	    for (ps1 = (unsigned char *) (str1 - ((x = o->rep) - (o->beg))),
		 xm = x + (o->length);
		 x < xm; x++, ps++)
		*(ps1 + *(pg + d * x)) = *ps;
	if (--(pt->dg))
	    lev++;
	if ((dg = (tree + (x = *(pg + d * (u + r)) - r))->dg) > 0) {
	    bcopy(str1, str, ls);
	    lo = lev;
	    *(stk + lev) = x;
	    if (dg > 1)
		writvirt(str, lev);
	} else
	    lev--;
	if (mode == 2)
	    for (i = 0; i < ls1; i++)
		*(str1 + i) = *(str1 + rep[i]);
	lseek(foc, beg + lstr + lstr1 * (x - 1), SEEK_SET);
	xwrite(foc, (void *) str1, ls1);
    }
    free(str);
    free(stk);
    free(tree);
    free(g);
    frevirt();
}

static void prtsym(ns, sym)
    int ns, *sym;
{
    char str[100], s;
    int i, l, b, u;

    Sprintf(str, "reflexive suborbits:");
    l = strlen(str);
    for (u = i = 0; i < ns; i++)
	if (sym[i] == -1) {
	    b = l;
	    u = 1;
	    Sprintf(str + l, "%d,", i);
	    if ((l = strlen(str)) > 75) {
		s = str[b];
		str[b] = '\0';
		prt(3, "%s\n", str);
		str[0] = s;
		bcopy(str + b + 1, str + 1, (l = l - b));
	    }
	}
    if (u) {
	str[l - 1] = '\0';
	prt(3, "%s\n", str);
    }
    Sprintf(str, "symmetrical suborbits:");
    l = strlen(str);
    for (u = i = 0; i < ns; i++)
	if (sym[i] == i) {
	    b = l;
	    u = 1;
	    Sprintf(str + l, "%d,", i);
	    if ((l = strlen(str)) > 75) {
		s = str[b];
		str[b] = '\0';
		prt(3, "%s\n", str);
		str[0] = s;
		bcopy(str + b + 1, str + 1, (l = l - b));
	    }
	}
    if (u) {
	str[l - 1] = '\0';
	prt(3, "%s\n", str);
    }
    Sprintf(str, "pairs of antisymmetrical suborbits:");
    l = strlen(str);
    for (u = i = 0; i < ns; i++)
	if (sym[i] > i) {
	    b = l;
	    u = 1;
	    Sprintf(str + l, "(%d,%d)", i, sym[i]);
	    if ((l = strlen(str)) > 75) {
		s = str[b];
		str[b] = '\0';
		prt(3, "%s\n", str);
		str[0] = s;
		bcopy(str + b + 1, str + 1, (l = l - b));
	    }
	}
    if (u)
	prt(3, "%s\n", str);
    Sprintf(str, "antisymmetrical suborbits:");
    l = strlen(str);
    for (u = i = 0; i < ns; i++)
	if (sym[i] == -2) {
	    b = l;
	    u = 1;
	    Sprintf(str + l, "%d,", i);
	    if ((l = strlen(str)) > 75) {
		s = str[b];
		str[b] = '\0';
		prt(3, "%s\n", str);
		str[0] = s;
		bcopy(str + b + 1, str + 1, (l = l - b));
	    }
	}
    if (u) {
	str[l - 1] = '\0';
	prt(3, "%s\n", str);
    }
    prt(3, "\n");
}

static void outsym(mode, no, lorb, matr, ns, asym, lh)
    unsigned char mode;
    unsigned int no, *lorb, ns, asym, lh;
    char *matr;
{
    unsigned int i, j, lstr, x, y, z, u;
    int sym[NRMAX];
    long beg;
    unsigned char *str, s;
    struct {
	long beg;
	unsigned int lstr, adr[NOMAX];
    } tab[NOMAX];

    for (i = 0; i < ns; i++)
	sym[i] = -2;
    for (beg = lh, i = 0; i < no;
	 beg += lstr * ((long) lorb[i++])) {
	for (tab[i].beg = beg, lstr = j = 0; j < no;
	     lstr += (matr[no * i + j]) ? lorb[j] : 0, j++)
	    tab[i].adr[j] = lstr;
	tab[i].lstr = lstr;
    }
    for (i = 0; i < no; i++) {
	if ((lstr = tab[i].lstr) > 0) {
	    if((str = Getmem(lstr, unsigned char)) == NULL) {
	      prt(3, "Out of memory\n");
	      longjmp(jb, 1);
	    }
	    lseek(foc, tab[i].beg, SEEK_SET);
	    read0(foc, str, lstr);
	    for (j = 0; j < no; j++)
		if (matr[no * i + j] && matr[no * j + i])
		    for (y = lorb[j], z = tab[i].adr[j], x = 0; x < y; x++)
			if (sym[u = *(str + z + x)] == -2) {
			  if (i == j && !x)
			    sym[u] = -1;
			  else {
			    lseek(foc, tab[j].beg + ((long) tab[j].lstr) *
				  ((mode == 3) ? x : 1) +
				  ((mode == 3) ? tab[j].adr[i] : (x - 1) * ns),
				  SEEK_SET);
			    read0(foc, &s, sizeof(s));
			    sym[u] = s;
			  }
			}
	    free(str);
	}
    }
    lseek(foc, (long) asym, SEEK_SET);
    if (write(foc, sym, ns * sizeof(*sym)) <= 0) {
	prt(3, "output error on file of colour graph : %s\n", strerror(errno));
	longjmp(jb, 1);
    }
    prtsym(ns, sym);
}

static void cgr(ng, gens, no, lorb, mode, matr)
    unsigned char mode, matr[];
    unsigned int ng, *gens, no, lorb[];
{
    unsigned int i, j, k, m, r, rr, s, ns, b, asym, lhead;
    struct point *points;
    struct orbit orb[NOMAX], *o;
    long beg;
    int f;

    if (mode > 1)
	if ((f = open("cgr#1.tmp", O_RDWR | O_CREAT, 0666)) < 0) {
	  prt(3, "open error on work file cgr#1.tmp : %s\n", strerror(errno));
	  longjmp(jb, 1);
	}
    for (k = r = i = 0; i < no; r += lorb[i++]) {
	for (o = orb, b = rr = ns = s = j = 0; j < no; rr += lorb[j++])
	    if ((m = matr[no * i + j]) > 0) {
		o->num = j;
		o->rep = rr;
		o->beg = b;
		b += lorb[j];
		o->sc = 0;
		s += (o->length = o->rank = lorb[j]);
		(o++)->rank1 = m;
		ns++;
	    }
	if (ns) {
	    if ((points = Getmem(s, struct point)) == NULL) {
		prt(3, "not enough space for points of orbit %u\n", i + 1);
		longjmp(jb, 1);
	    }
	    fixpoint(ng, gens, lorb[i], r, s, points, ns, orb);
	    prsubd(mode, i, r, ns, orb, points, k);
	    if (mode > 1)
		writp(f, (char *) points, ((long) s) * sizeof(*points));
	    free(points);
	    for (j = 0; j < ns; j++)
		k += (matr[no * i + (orb + j)->num] = (orb + j)->rank);
	}
    }
    if (mode > 1) {
	outhead(mode, no, lorb, matr, k, &asym, &lhead);
	lseek(f, 0l, SEEK_SET);
	for (beg = lhead, k = r = i = 0; i < no; r += lorb[i++]) {
	    for (o = orb, b = rr = ns = s = j = 0; j < no; rr += lorb[j++])
		if (matr[no * i + j]) {
		    o->num = j;
		    o->rep = rr;
		    o->beg = b;
		    b += lorb[j];
		    s += ((o++)->length = lorb[j]);
		    ns++;
		}
	    if (ns) {
	        if((points = Getmem(s, struct point)) == NULL) {
		  prt(3, "Out of memory\n");
		  longjmp(jb, 1);
		}
		readp(f, points, ((long) s) * sizeof(*points));
		outcgr(ng, gens, r, mode, ns, orb, lorb[i], s, points, k, beg);
		for (j = 0; j < ns; j++)
		    k += matr[no * i + (orb + j)->num];
		beg += ((long) s) * lorb[i];
		free(points);
		if (no > 1)
		    prt(3, "%u row of blocks of ", i + 1);
		prt(3, "colour graph is saved\n");
	    }
	}
	(void) close(f);
	(void) unlink("cgr#1.tmp");
	prt(3, "\n");
	outsym(mode, no, lorb, matr, k, asym, lhead);
    }
}

static int inpopt(no, mode, matr, poserr)
    unsigned char *mode, matr[];
    unsigned int no;
    long *poserr;
{
    int i, i1, j, r, v;
    char s;
    unsigned char *m, *em;

    readstr(fio, flio);
    for (*mode = (floc) ? 3 : 0, em = (m = matr) + no * no; m < em; m++)
	*m = 1;
    if (strequal("\n"))
	return (1);
    for (r = 1, j = 0; (s = nxtsym()); j++)
	switch (r) {
	case 1:
	    if (s == 'o' && !floc && !j) {
		*mode = 1;
		break;
	    }
	    if (s == 'i' && floc && no == 1 && !j) {
		*mode = 2;
		break;
	    }
	    for (m = matr; m < em; m++)
		*m = 0;
	    if (s == ',' && j == 1)
		break;
	    if (s == 't' && no > 1) {
		for (i = 0; i < no; i++)
		    for (i1 = i; i1 < no; i1++)
			matr[no * i + i1] = 1;
		r = 12;
		break;
	    }
	    if (s == 'd' && no > 1) {
		for (i = 0; i < no; i++)
		    matr[(no + 1) * i] = 1;
		r = 12;
		break;
	    }
	    if (s == '(' && no > 1 && j != 1) {
		r = 3;
		break;
	    }
	    if (s >= '0' && s <= '9' && j != 1) {
		r = 11;
		*(m = matr) = s - '0';
		break;
	    }
	    *poserr = j;
	    return (0);
	case 2:
	    if (s == '(') {
		r = 3;
		break;
	    }
	    *poserr = j;
	    return (0);
	case 3:
	    if (s > '0' && s <= '9' && (i = s - '0') <= no) {
		r = 4;
		break;
	    }
	    *poserr = j;
	    return (0);
	case 4:
	    if (s == '-') {
		r = 5;
		break;
	    }
	    if (s >= '0' && s <= '9' && (i = 10 * i + (s - '0')) <= no)
		break;
	    *poserr = j;
	    return (0);
	case 5:
	    if (s > '0' && s <= '9' && (i1 = s - '0') <= no) {
		r = 6;
		break;
	    }
	    *poserr = j;
	    return (0);
	case 6:
	    if (s == ',' || s == ')') {
		matr[(i - 1) * no + i1 - 1] = 1;
		r = (s == ',') ? 5 : 2;
		break;
	    }
	    if (s == '[') {
		r = 7;
		break;
	    }
	    if (s >= '0' && s <= '9' && (i1 = 10 * i1 + (s - '0')) <= no)
		break;
	    *poserr = j;
	    return (0);
	case 7:
	    if (s > '0' && s <= '9') {
		v = s - '0';
		r = 8;
		break;
	    }
	    *poserr = j;
	    return (0);
	case 8:
	    if (s == ']') {
		matr[(i - 1) * no + i1 - 1] = v;
		r = 9;
		break;
	    }
	    if (s >= '0' && s <= '9' && (v = 10 * v + (s - '0')) < NRMAX)
		break;
	    *poserr = j;
	    return (0);
	case 9:
	    if (s == ',') {
		r = 5;
		break;
	    }
	    if (s == ')') {
		r = 2;
		break;
	    }
	    *poserr = j;
	    return (0);
	case 10:
	    if (s >= '0' && s <= '9') {
		*m = s - '0';
		r = 11;
		break;
	    }
	    *poserr = j;
	    return (0);
	case 11:
	    if (s == ',') {
		r = 10;
		if ((++m) == em) {
		    *poserr = j;
		    return (0);
		} break;
	    }
	    if (s >= '0' && s <= '9' && (*m = 10 * (*m) + (s - '0')) <= NRMAX)
		break;
	case 12:
	    *poserr = j;
	    return (0);
	}
    *poserr = j;
    return (((r == 1 && s != ',') || r == 2 || r >= 10) ? 1 : 0);
}

static void input(ggen, no, lorb, mode, matr)
    struct ggen *ggen;
    unsigned int *no, lorb[];
    unsigned char *mode, matr[];
{
    long poserr;

    readgp(&ingpf, ggen);
    orbits(ggen->n, ggen->ng, ggen->gens, no, lorb);
    while (1) {
	if (flio) {
	    prt(3, "options(");
	    if (*no == 1)
		prt(3, (floc) ? "i,rank):\n" : "o,rank):\n");
	    else
		prt(3, (floc) ? "t,d,list or ranks):\n" :
		    "o,t,d,ot,od,list or ranks):\n");
	}
	if (inpopt(*no, mode, matr, &poserr))
	    break;
	prt(3, "error in position %ld\n", poserr);
	if (!(flio & KBDBIT))
	    longjmp(jb, 1);
    }
    if (flio)
	prt(3, "\n");
}

void cleanup()
{
    if (!(flig & KBDBIT))
	fclose(fig);
    if (!(flio & KBDBIT))
	fclose(fio);
    if (floc)
	close(foc);
}

static void openf(na, pa)
    char *pa[];
    int na;
{
    if(!openfile("input of group from %s\n",
		 &ingpf, (na > 2) ? pa[2] : "*", "gen", "rt"))
      longjmp(jb, 1);

    if(!openfile("input of options from %s\n",
		 &inoptf, (na > 3) ? pa[3] : "*", "opt", "rt"))
      longjmp(jb, 1);

    if (na > 4) {
        char buf[1000];
	Sprintf(buf, "%s.cgr", pa[4]);
	floc = 1;
	prt(3, "output of colour graph to file %s\n", buf);
	if ((foc = open(buf, O_RDWR | O_CREAT | O_TRUNC, 0666)) < 0) {
	    prt(3, "open error on file %s : %s\n", buf, strerror(errno));
	    longjmp(jb, 1);
	}
    } else
	floc = 0;
    prt(3, "\n");
}

void Main(int na, char **pa)
{
    unsigned char mode, matr[NOMAX * NOMAX];
    unsigned int no, lorb[NOMAX];
    struct ggen ggen;

    prt(3,
	"\n* construction of 2-orbits of permutation group *\n\n");
    openf(na, pa);
    input(&ggen, &no, lorb, &mode, matr);
    cgr(ggen.ng, ggen.gens, no, lorb, mode, matr);
}
