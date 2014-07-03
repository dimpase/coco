/*
 * COCO - Unix version 1.0 - aeb - 920530
 *
 * inm.c
 */
#include <stdio.h>
#include <setjmp.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include "def.h"
#include "prt.h"
#include "outgp.h"
#include "common.h"
#include "readstr.h"

#define NOMAX 15
#define NSMAX 256
#define NHASH 256

jmp_buf jb;
struct file inoptf, outf;
#define fio inoptf.fd
#define foc outf.fd
#define flio inoptf.flags
#define floc outf.flags
int fic;

struct orb {
    unsigned int lorb, lstr;
    long beg;
};

struct tab {
    unsigned char orb;
    unsigned int lb, ba, bc, nsub;
};

struct ic {
    unsigned char i, j, k;
    unsigned int lambda;
    struct ic *seq;
};

static void outsym(ns, sym)
    unsigned int ns;
    int *sym;
{
    char str[100];
    unsigned int b;
    int i;

    for (str[0] = '\0', b = i = 0; i < ns; i++)
	if (sym[i] > i) {
	    Sprintf(str + b, "(%d,%d)", i, sym[i]);
	    b = partline(str, b, foc);
	}
    Fprintf(foc, "%s\n", str);
}

static void outhead(no, matr, ns, sym)
    unsigned int no, ns;
    int *sym;
    unsigned char *matr;
{
    unsigned int b;
    unsigned char *pm, *em;
    char str[100];

    Fprintf(foc, "%u\n", no);
    for (b = 0, em = (pm = matr) + no * no; pm < em; pm++) {
	if (pm > matr)
	    Sprintf(str + (b++), ",");
	Sprintf(str + b, "%u", *pm);
	b = partline(str, b, foc);
    }
    Fprintf(foc, "%s\n", str);
    outsym(ns, sym);
}

static void outconst(nc, ic)
    long nc;
    struct ic *ic;
{
    struct ic *pc;
    char str[100];
    unsigned int b;
    long i;

    Fprintf(foc, "%ld\n", nc);
    for (pc = ic, b = i = 0; i < nc; i++, pc++) {
	Sprintf(str + b, "(%u,%u,%u-%u)", pc->i, pc->j, pc->k, pc->lambda);
	b = partline(str, b, foc);  /* originally: without continuation *'s */
    }
    Fprintf(foc, "%s\n", str);
}

static void prconst(ic, ec)
    struct ic *ic, *ec;
{
    struct ic *pc;
    char str[100];
    unsigned int b;

    for (b = 0, pc = ic; pc < ec; pc++) {
	Sprintf(str + b, " p(%u,%u,%u)=%u,", pc->i, pc->j, pc->k, pc->lambda);
	b = partline(str, b, stdout);
    }
    str[b - 1] = 0;
    prt(3, "%s\n\n", str);
}

static int comp(pc1, pc2)
    struct ic *pc1, *pc2;
{
    unsigned char i1, i2, j1, j2;

    i1 = pc1->i;
    i2 = pc2->i;
    j1 = pc1->j;
    j2 = pc2->j;
    if (i1 < i2 || (i1 == i2 && (j1 < j2 || (j1 == j2 && pc1->k < pc2->k))))
	return -1;
    else
	return 1;
}

static void inconst(mode, orba, stra, ltab, tab, orbc, lc, lstrc, begc, ca)
    unsigned int orba, ltab, orbc, lc, lstrc, ca;
    unsigned char mode, *stra;
    struct tab *tab;
    long begc;
{
    char sc[NSMAX];
    unsigned int i, im, j, k, l, nsub, x, f, b[NSMAX], *pb, *eb;
    unsigned char *psa, *psc, *strc;
    struct tab *pt, *et;
    struct ic *hash[NHASH], *ic, *pc, *ec, *eec;
    long nc;

    for (i = 0; i < NSMAX; i++)
	sc[i] = 0;
    for (nsub = 0, et = (pt = tab) + ltab; pt < et; (pt++)->nsub = x)
	for (psa = stra + pt->ba, im = pt->lb, x = i = 0; i < im; i++, psa++)
	    if (!sc[*psa]) {
		b[nsub++] = i;
		x++;
		sc[*psa] = 1;
	    }
    for (i = 0; i < NHASH; i++)
	hash[i] = NULL;
    if ((strc = Getmem(lstrc, unsigned char)) == NULL) {
	prt(3, "not enough space for second row of color graph\n");
	longjmp(jb, 1);
    }

    /* %% figure out how much memory is actually needed */
    ec = ic = (struct ic *) getmem(65535, sizeof(*ic));
    eec = ec + 65535;
    (void) lseek(fic, begc, SEEK_SET);
    for (nc = l = 0; l < lc; l++) {
	read0(fic, (char *)strc, (int)((mode || !l) ? lstrc : nsub));
	for (i = *(stra + ca + l), pb = b, pt = tab; pt < et; pt++)
	    for (psa = stra + pt->ba, psc = strc + pt->bc, eb = pb + pt->nsub;
		 pb < eb; pb++) {
	      k = *(psa + *pb);
	      j = *(psc + ((mode || !l) ? (*pb) : (pb - b)));
	      f = ((((i + (j << 4) + (k << 8)) * 0x983F69B1) & 0xFFF000) >> 12)
		% NHASH;
	      for (pc = hash[f];
		   pc != NULL && !(pc->i == i && pc->j == j && pc->k == k);
		   pc = pc->seq);
	      if (pc == NULL)
		if (ec == eec) {
		  prt(3, "not enough space for intersection numbers\n");
		  longjmp(jb, 1);
		} else {
		  ec->i = i;
		  ec->j = j;
		  ec->k = k;
		  ec->lambda = 1;
		  ec->seq = (struct ic *) hash[f];
		  hash[f] = ec++;
		  nc++;
		}
	      else
		(pc->lambda)++;
	    }
    }
    prt(3, "there are %ld nonzero intersection numbers with i of type %d,%d\n",
	nc, orba + 1, orbc + 1);
    if (floc & VERBOSE) {
	qsort((char *) ic, (int) nc, sizeof(*ic), comp);
	prconst(ic, ec);
    }
    if (floc & EXISTS)
	outconst(nc, ic);
    free(strc);
    free(ic);
}

static void fixpt(mode, orba, no, orbs, matr, sc)
    unsigned int orba, no;
    unsigned char mode, *matr, *sc;
    struct orb orbs[];
{
    unsigned int lstra, ltab, orbb, orbc, ba, bc, ca, lb;
    struct tab tab[NOMAX];
    unsigned char *str, *pma, *pmc, *ps, *ps1;

    if ((str = Getmem((lstra = orbs[orba].lstr), unsigned char)) == NULL) {
	prt(3, "not enough space for first row of colour graph\n");
	longjmp(jb, 1);
    }
    (void) lseek(fic, orbs[orba].beg, SEEK_SET);
    read0(fic, (char *)str, (int)lstra);
    for (pma = (pmc = matr) + orba * no, ps = sc, ca = orbc = 0; orbc < no;
	 ca += (*(pma + orbc)) ? orbs[orbc++].lorb : 0, pmc += no, ps++) {
	for (ps1 = ps, ba = bc = ltab = orbb = 0; orbb < no; ps1 += no,
	     lb = orbs[orbb].lorb, ba += (*(pma + orbb)) ? lb : 0,
	     bc += (*(pmc + orbb++)) ? lb : 0)
	    if (*ps1) {
		tab[ltab].orb = orbb;
		tab[ltab].ba = ba;
		tab[ltab].bc = bc;
		tab[ltab++].lb = orbs[orbb].lorb;
	    }
	if (ltab)
	    inconst(mode, orba, str, ltab, tab, orbc, orbs[orbc].lorb,
		    orbs[orbc].lstr, orbs[orbc].beg, ca);
    }
    free(str);
}

static void inc(mode, no, lorb, matr, sc, ns, sym)
    unsigned int no, lorb[], ns;
    unsigned char mode, *matr, *sc;
    int *sym;
{
    unsigned int no2, orba, i, j;
    unsigned char *pm, *ps, *ps1, *es;
    long beg, lstr;
    struct orb orbs[NOMAX], *po;

    if (floc & EXISTS)
	outhead(no, matr, ns, sym);
    beg = sizeof(mode) + sizeof(no) + no * sizeof(*lorb) +
	(no2 = no * no) * sizeof(*matr) + ns * sizeof(*sym);
    for (po = orbs, pm = matr, i = 0; i < no;
	 (po++)->lstr = lstr, beg += lstr * lorb[i++])
      for (po->lorb = lorb[i], po->beg = beg, lstr = j = 0; j < no; j++, pm++)
	lstr += (*pm) ? lorb[j] * sizeof(char) : 0;
    if (floc & VERBOSE)
	prt(3, "\nintersection numbers p(i,j,k):\n\n");
    for (ps = sc, orba = 0; orba < no; orba++, ps += no2) {
	for (es = (ps1 = ps) + no2; ps1 < es && !(*ps1); ps1++);
	if (ps1 < es)
	    fixpt(mode, orba, no, orbs, matr, ps);
    }
}

static unsigned int inplo(no, lorb)
    unsigned int no, lorb[];
{
    unsigned int i, n;

    for (n = i = 0; i < no; i++) {
	if (lorb[i] == 0) {
	    prt(3, "zero block size?\n");
	    longjmp(jb, 1);
	}
	n += lorb[i];
    }
    return n;
}

static void inpmtr(no, matr, sc)
    unsigned int no;
    unsigned char *matr, *sc;
{
    unsigned int no2, l;
    unsigned char *pm, *pm1, *pm2, *pm3, *pm4, *em, *em1, *ps, *es;

    for (no2 = no * no, es = (ps = sc) + no2 * no; ps < es; ps++)
	*ps = 0;
    for (em = (pm = matr) + no2, ps = sc, l = 0; pm < em; pm += no)
	for (em1 = (pm1 = pm) + no, pm4 = matr; pm1 < em1; pm1++, pm4++)
	    for (pm2 = pm, pm3 = pm4; pm2 < em1; pm2++, pm3 += no, ps++)
		if (*pm1 && *pm2 && *pm3)
		    *ps = l = 1;
    if (!l) {
	prt(3, "no intersection number may be computed");
	prt(3, "from the given parts of colour graph\n");
	longjmp(jb, 1);
    }
}

static void inpsym(no, matr, ns, sym)
    unsigned int no, *ns;
    unsigned char *matr;
    int *sym;
{
    unsigned int i;

    for (*ns = i = 0; i < no * no; i++)
	(*ns) += matr[i];
    read0(fic, (char *)sym, (int)(*ns) * sizeof(*sym));
}

static int inpsc(no, matr, sc, poserr)
    unsigned char *matr, *sc;
    unsigned int no, *poserr;
{
    unsigned int i, im, j, k, r, n1, n2, n3, x[3];
    char s;

    readstr(fio, flio);
    if (strequal("\n"))
	return (1);
    if (strequal("ndg\n") == 0) {
	for (n1 = no * (no + 1) + 1, i = 0; i < no; i++)
	    sc[i * n1] = 0;
	for (im = no * no * no, i = 0; i < im && !sc[i]; i++);
	if (i == im) {
	    prt(3, "no intersection number can be computed\n");
	    *poserr = 0;
	    return (0);
	}
	return (1);
    }
    if (strequal("nrf\n") == 0) {
	for (i = 0; i < no; i++) {
	    n1 = i * no * (no + 1);
	    n2 = i * no * no;
	    n3 = n2 + i;
	    for (j = 0; j < no; j++)
		sc[n1 + j] = sc[n3 + j * no] = sc[n2 + j * (no + 1)] = 0;
	}
	for (im = no * no * no, i = 0; i < im && !sc[i]; i++);
	if (i == im) {
	    prt(3, "no intersection number can be computed\n");
	    *poserr = 0;
	    return (0);
	}
	return (1);
    }
    for (im = no * no * no, i = 0; i < im; i++)
	sc[i] = 0;
    for (r = 1, j = 0; (s = nxtsym()); j++)
	switch (r) {
	case 1:
	    if (s == '(') {
		r = 2;
		k = 0;
		break;
	    }
	    *poserr = j;
	    return (0);
	case 2:
	    if (s > '0' && s <= '9' && (x[k] = s - '0') <= no) {
		r = 3;
		break;
	    }
	    *poserr = j;
	    return (0);
	case 3:
	    if (s == ',') {
		if (k == 2) {
		    *poserr = j;
		    return (0);
		}
		k++;
		r = 2;
		break;
	    }
	    if (s == ')') {
		if (k != 2) {
		    *poserr = j;
		    return (0);
		}
		for (i = 0; i < 3; x[i++]--);
		if (!matr[x[0] * no + x[1]] || !matr[x[0] * no + x[2]] ||
		    !matr[x[2] * no + x[1]]) {
		    prt(3, "no such parts of colour graph\n");
		    *poserr = j;
		    return (0);
		}
		sc[no * (no * x[0] + x[1]) + x[2]] = 1;
		r = 1;
		break;
	    }
	    if (s >= '0' && s <= '9' && (x[k] = 10 * x[k] + (s - '0')) <= no)
		break;
	    *poserr = j;
	    return (0);
	}
    *poserr = j - 1;
    return ((r == 1) ? 1 : 0);
}

static void input(mode, no, lorb, matr, sc, ns, sym)
    unsigned int *no, *lorb, *ns;
    unsigned char *mode, *matr, *sc;
    int *sym;
{
    unsigned int n, poserr;

    (void) lseek(fic, 0l, SEEK_SET);
    read0(fic, (char *) mode, sizeof(*mode));
    if (*mode != 0 && *mode != 1) {
	prt(3, "incorrect mode\n");
	longjmp(jb, 1);
    }
    read0(fic, (char *)no, sizeof(*no));
    if (*no == 0 || *no > NOMAX) {
	prt(3, "incorrect number of blocks\n");
	longjmp(jb, 1);
    }
    if (!(*mode) && *no > 1) {
	prt(3, "mode 0 admissible for 1 block only\n");
	longjmp(jb, 1);
    }
    read0(fic, (char *)lorb, (int)(*no) * sizeof(*lorb));
    n = inplo(*no, lorb);
    read0(fic, (char *)matr, (int)((*no) * (*no) * sizeof(*matr)));
    inpmtr(*no, matr, sc);
    inpsym(*no, matr, ns, sym);
    if (*no == 1)
	prt(3, "homogeneous ");
    prt(3, "colour graph of rank %u on %u vertices\n", *ns, n);
    if (*no > 1)
	outlorb(*no, lorb);
    prt(3, "\n");
    if (*no > 1) {
	while (1) {
	    if (flio)
		prt(3, "options (nrf, ndg or list):\n");
	    if (inpsc(*no, matr, sc, &poserr))
		break;
	    prt(3, "error in position %u\n", poserr);
	    if (!(flio & KBDBIT))
		longjmp(jb, 1);
	}
	if (flio)
	    prt(3, "\n");
    }
}

void cleanup()
{

    close(fic);
    if (!(flio & KBDBIT))
	fclose(fio);
    if (floc)
	fclose(foc);
}

static void openf(na, pa)
    int na;
    char *pa[];
{
    char buf[1000];

    prt(3, "\n* calculation of intersection numbers *\n\n");
    if (na <= 2) {
	prt(3, "no input file for color graph\n");
	longjmp(jb, 1);
    }

    Sprintf(buf, "%s.cgr", pa[2]);
    prt(3, "input of colour graph from file %s\n", buf);
    if ((fic = open(buf, O_RDONLY)) < 0) {
	prt(3, "open error on file %s : %s\n", buf, strerror(errno));
	longjmp(jb, 1);
    }

    if(!openfile("input of options from %s\n", &inoptf,
		 (na > 3) ? pa[3] : "*", "opt", "rt"))
      longjmp(jb, 1);

    floc = 0;
    if (na > 4) {
      if(!openfile("output of intersection numbers to file %s\n",
		   &outf, pa[4], "nrs", "wt"))
	longjmp(jb, 1);
    }

    prt(3, "\n");
}

void Main(na, pa)
    char **pa;
    int na;
{
    unsigned char mode, matr[NOMAX * NOMAX], sc[NOMAX * NOMAX * NOMAX];
    unsigned int no, lorb[NOMAX], ns;
    int sym[NSMAX];

    openf(na, pa);
    input(&mode, &no, lorb, matr, sc, &ns, sym);
    inc(mode, no, lorb, matr, sc, ns, sym);
}
