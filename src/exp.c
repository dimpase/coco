/*
 * COCO - Unix version 1.0 - aeb - 920530
 *
 * exp.c
 */

/* exp.c: construct the wreath product of two permutation groups */
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <string.h>
#include <errno.h>
#include "def.h"
#include "prt.h"
#include "ingp.h"
#include "outgp.h"
#include "common.h"

#define N1MAX 1000     /* need N1MAX >= N2MAX */
#define N2MAX 20
#define NOMAX 15

jmp_buf jb;
struct file ingp1f, ingp2f, outgpf, outmapf;
#define fig1 ingp1f.fd
#define fig2 ingp2f.fd
#define fog outgpf.fd
#define fom outmapf.fd
#define flig1 ingp1f.flags
#define flig2 ingp2f.flags
#define flog outgpf.isopen
#define flom outmapf.isopen

/* find number of orbits and put representatives in rep[] */
static void orbits(ggen, rep, repsz)
    struct ggen *ggen;
    unsigned int *rep, repsz;
{
    unsigned int n, ng, i, j, k, s, que[N1MAX], bq, eq;
    char todo[N1MAX];

    n = ggen->n;
    ng = ggen->ng;
    for (i = 0; i < n; i++)
	todo[i] = 1;
    for (ggen->no = i = 0; i < n; i++)
	if (todo[i]) {
	    if (ggen->no == repsz) {
		prt(3, "number of orbits is greater than %u\n", repsz);
		longjmp(jb, 1);
	    }
	    for (todo[i] = bq = 0, rep[(ggen->no)++] = *que = i, eq = 1;
		 bq < eq; bq++)
		for (s = que[bq], j = 0; j < ng; j++)
		    if (todo[k = (ggen->gens)[ng * s + j]]) {
			todo[k] = 0;
			que[eq++] = k;
		    }
	}
    if (ggen->no == 1)
	prt(3, "transitive group\n\n");
    else
	prt(3, "group with %u orbits\n\n", ggen->no);
}

/* make orbits consecutive and return their number */
static int shuffle(n,ng,gens,perm)
    unsigned int n, ng, *gens, *perm;
{
    unsigned int i, j, k, s, no, *inv, *w;
    unsigned int bq, bq1, eq, lorb[NOMAX], d;
    unsigned char *todo;

    if ((todo = Getmem(n, unsigned char)) == NULL) {
        prt(3, "No room for shuffling.\n");
	longjmp(jb, 1);
    }

    d = ng;
    w = (inv = perm + n) + n;
    for (j = 0; j < n; j++)
	todo[j] = 1;
    for (bq = no = j = 0; j < n; j++)
	if (todo[j]) {
	    todo[j] = 0;
	    if (no == NOMAX) {
		prt(3, "number of orbits is greater than %u\n", NOMAX);
		longjmp(jb, 1);
	    }
	    for (perm[bq] = j, eq = (bq1 = bq) + 1; bq < eq; bq++)
		for (s = perm[bq], i = 0; i < ng; i++)
		    if (todo[k = gens[d * s + i]]) {
			todo[k] = 0;
			perm[eq++] = k;
		    }
	    lorb[no++] = eq - bq1;
	}
    for (j = 0; j < n; j++)
	inv[perm[j]] = j;
    for (i = 0; i < ng; i++) {
	for (j = 0; j < n; j++)
	    w[j] = inv[gens[d * perm[j] + i]];
	for (j = 0; j < n; j++)
	    gens[d * j + i] = w[j];
    }
    free((char *) todo);
    return(no);
}

static void outmap(n, n1, n2, perm)
    unsigned int n, n1, n2, *perm;
{
    unsigned int j, k, s;

    for (j = 0; j < n; j++) {
	Fprintf(fom, "%u.(", j);
	for (s = perm[j], k = 0; k < n2; k++, s /= n1) {
	    if (k)
		Fprintf(fom, ",");
	    Fprintf(fom, "%u", s % n1);
	}
	Fprintf(fom, ")\n");
    }
}

static void expon(n1, ng1, gens1, n2, ng2, no2, gens2, rep)
    unsigned int n1, ng1, *gens1, n2, ng2, no2, *gens2, *rep;
{
    unsigned int i, j, k, p, u, s, n, ng, *gens,
      a[N2MAX], b[N2MAX], *perm;
    unsigned int no, lorb[NOMAX], d;

    prt(3, "wreath product:\n");
    ng = ng1 * no2 + ng2;

    for (n = 1, i = 0; i < n2; i++, n *= n1);
    if ((gens = Getmem(n * ng, unsigned int)) == NULL ||
	(perm = Getmem(3 * n, unsigned int)) == NULL) {
	prt(3, "not enough space for exponentiation\n");
	longjmp(jb, 1);
    }

    d = ng;
    for (k = 0; k < no2; k++) {
	for (p = rep[k], u = 1, i = 0; i < p; i++, u *= n1);
	for (i = 0; i < ng1; i++)
	    for (j = 0; j < n; j++) {
	        s = (j / u) % n1;
		gens[d * j + k * ng1 + i] = j - s * u + gens1[s * ng1 + i] * u;
	    }
    }
    for (i = 0; i < ng2; i++) {
	for (j = 0; j < n; gens[d * (j++) + ng1 * no2 + i] = s) {
	    for (s = j, k = 0; k < n2; k++, s /= n1)
		a[k] = s % n1;
	    for (k = 0; k < n2; k++)
		b[k] = a[gens2[k * ng2 + i]];
	    for (s = k = 0; k < n2; s = s * n1 + b[n2 - (++k)]);
	}
    }

    /*
     * Here we have generators for our group, but elsewhere it is assumed
     * that blocks are consecutive, so we have to rearrange points.
     */
    no = shuffle(n,ng,gens,perm);

    if (no == 1)
	prt(3, "transitive ");
    prt(3, "group of degree %u with %u generators\n", n, ng);
    if (no > 1)
	outlorb(no, lorb);
    if (flog)
	outgens(n, ng, gens, fog);
    if (flom)
	outmap(n, n1, n2, perm);
    free(gens);
    free(perm);
}

static void openf(na, pa)
    char *pa[];
    int na;
{
    if(!openfile("input of first group from %s\n",
		 &ingp1f, (na > 2) ? pa[2] : "*", "gen", "rt"))
      longjmp(jb, 1);

    if(!openfile("input of second group from %s\n",
		 &ingp2f, (na > 3) ? pa[3] : "*", "gen", "rt"))
      longjmp(jb, 1);

    if (na > 4) {
        if(!openfile("output of resulting group to file %s\n",
		     &outgpf, pa[4], "gen", "wt"))
	  longjmp(jb, 1);
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

void cleanup()
{
    if (!(flig1 & KBDBIT))
	fclose(fig1);
    if (!(flig2 & KBDBIT))
	fclose(fig2);
    if (flog)
	fclose(fog);
    if (flom)
	fclose(fom);
}

void Main(int na, char **pa)
{
    unsigned int rep[NOMAX];
    struct ggen ggen1, ggen2;

    prt(3, "\n* exponentiation of permutation groups *\n\n");
    openf(na, pa);
    prt(3, "first group:\n");
    readgp(&ingp1f, &ggen1);
    orbits(&ggen1, rep, NOMAX);
    prt(3, "second group:\n");
    readgp(&ingp2f, &ggen2);
    orbits(&ggen2, rep, NOMAX);
    expon(ggen1.n, ggen1.ng, ggen1.gens,
	  ggen2.n, ggen2.ng, ggen2.no, ggen2.gens, rep);
}
