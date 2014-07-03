/*
 * COCO - Unix version 1.0 - aeb - 920530
 *
 * ingp.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include "def.h"
#include "prt.h"
#include "ingp.h"
#include "readstr.h"

extern jmp_buf jb;

static int inpgen(n, gen, delta, poserr)
    unsigned int n, *gen, delta, *poserr;
{
    char s;
    unsigned int i, k, r, x, x0, x1, *pg;
    long j, j1;

    for (i = 0; i < n; i++)
        gen[i * delta] = n;

    x1 = j1 = 0;
    for (r = 1, k = j = 0; (s = nxtsym()); j++) {
      switch (r) {
      case 1:
	/* cycle notation? */
	if (s == '(') {
	  r = 2;
	  i = 0;
	  break;
	}
	/* no, then explicit permutation */
	if (s >= '0' && s <= '9' && (x = s - '0') < n) {
	  r = 4;
	  break;
	}
	prt(3, "unexpected `%c' in generator\n", s);
	*poserr = j;
	return (0);
      case 2:
	if (s >= '0' && s <= '9' && (x = s - '0') < n) {
	  r = 3;
	  break;
	}
	prt(3, "unexpected `%c' in generator\n", s);
	*poserr = j;
	return (0);
      case 3:
	switch (s) {
	case ',':
	  if (i) {
	    if (*(pg = gen + delta * x1) == n)
	      *pg = x;
	    else {
	      prt(3, "%u occurs twice\n", x1);
	      *poserr = j1;
	      return (0);
	    }
	  } else
	    x0 = x;
	  x1 = x;
	  i++;
	  r = 2;
	  j1 = j - 1;
	  break;
	case ')':
	  if (i) {
	      if (*(pg = gen + delta * x1) == n)
		*pg = x;
	      else {
		  prt(3, "%u occurs twice\n", x1);
		  *poserr = j - 1;
		  return (0);
	      }
	  } else
	    x0 = x;
	  if (*(pg = gen + delta * x) == n)
	    *pg = x0;
	  else {
	    prt(3, "%u occurs twice\n", x);
	    *poserr = j - 1;
	    return (0);
	  }
	  r = 1;
	  break;
	default:
	  if (s < '0' || s > '9' || (x = 10 * x + (s - '0')) >= n) {
	    *poserr = j;
	    return (0);
	  }
	}
	break;
      case 4:
	/* explicit generator: x1,x2,...,xn; */
	switch (s) {
	case ';':
	  if(k != n-1) {
	    prt(3, "premature `;' in generator?\n");
	    *poserr = j;
	    return(0);
	  }
	  r = 1;
	  /* fall through */
	case ',':
	  *(gen + delta * k++) = x;
	  x = 0;
	  break;
	default:
	  if (s < '0' || s > '9' || (x = 10 * x + (s - '0')) >= n) {
	    prt(3, "unexpected `%c' in generator\n", s);
	    *poserr = j;
	    return (0);
	  }
	}
      }
    }
    for (pg = gen, i = 0; i < n; i++, pg += delta)
	if (*pg == n)
	    *pg = i;
    *poserr = j - 1;
    return ((r == 1) ? 1 : 0);
}

void readgp(struct file *file, struct ggen *ggen)
{
#define fig  file->fd
#define flig file->flags
    unsigned int n, ng, i, s, poserr;

    while (1) {
	if (flig)
	    prt(3, "degree of group:\n");
	readstr(fig, flig);
	if (!inpnum(&n))
	    prt(3, "error reading degree\n");
	else if (n <= 0)
	    prt(3, "bad degree\n");
	else
	    break;
	if (!(flig & KBDBIT))
	    longjmp(jb, 1);
    }
    ggen->n = n;
    while (1) {
	if (flig)
	    prt(3, "number of generators of group:\n");
	readstr(fig, flig);
	if (!inpnum(&ng))
	    prt(3, "error reading number of generators\n");
	else if (ng <= 0)
	    prt(3, "bad number of generators\n");
	else
	    break;
	if (!(flig & KBDBIT))
	    longjmp(jb, 1);
    }
    ggen->ng = ng;
    s = ng * sizeof(*(ggen->gens));
    ggen->gens = (unsigned int *) getmem(s, n);
    if (ggen->gens == NULL) {
	prt(3, "not enough space for input of generators\n");
	longjmp(jb, 1);
    }
    if (flig)
	prt(3, "generators of group:\n");
    for (i = 0; i < ng; i++)
	while (1) {
	    if (flig)
		prt(3, "%d.", i + 1);
	    readstr(fig, flig);
	    if (inpgen(n, ggen->gens + i, ng, &poserr))
		break;
	    prt(3, "error in position %ld\n", poserr);
	    if (!(flig & KBDBIT))
		longjmp(jb, 1);
	}
    if (flig)
	prt(3, "\n");
}
