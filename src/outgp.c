/*
 * COCO - Unix version 1.0 - aeb - 920530
 *
 * outgp.c
 */
#include <stdio.h>
#include <string.h>
#include "def.h"
#include "prt.h"
#include "outgp.h"

void outgens(unsigned int n, unsigned int ng, unsigned int *gens, FILE *f)
{
    char str[100];
    unsigned int i, j, d, x, l, ls, lb, lc, *pg, *pc;

    Fprintf(f, "%u\n%u\n", n, ng);
    d = ng;
    for (i = 0; i < ng; i++, gens++) {
	for (pg = gens, *str = 0, ls = j = 0; j < n; j++, pg += ng) {
	    for (l = 0, pc = pg, x = j; *pc < n && *pg != j;
		 x = *pc, *pc += n, pc = gens + d * x, l = 1) {
		if (x == j) {
		    Sprintf(str + ls, "(%u", x);
		    lb = ls;
		} else {
		    Sprintf(str + ls, ",%u", x);
		    lc = ls + 1;
		}
		if ((ls = strlen(str)) > 75) {
		    if (!lb)
			lb = lc;
		    ls = partline(str,lb,f);
		    lb = 0;
		}
	    }
	    if (l) {
		str[ls++] = ')';
		str[ls] = '\0';
	    }
	}
	Fprintf(f, "%s\n", str);

	/* repair gens - maybe superfluous at present */
	for(pc = gens, j = 0; j < n; j++, pc += ng)
	    if(*pc >= n)
	        *pc -= n;
    }
}

void outlorb(unsigned int no, unsigned int *lorb)
{
    unsigned i, ls;
    char str[100];

    Sprintf(str, "%u blocks of sizes ", no);
    for (ls = strlen(str), i = 0; i < no; i++) {
      if(i)
	str[ls++] = ',';
      Sprintf(str + ls, "%u", lorb[i]);
      ls = partline(str,ls,stdout);
    }
    prt(3, "%s\n", str);
}

