/*
 * COCO - Unix version 1.0 - aeb - 920530
 *
 * cosen.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include "def.h"
#include "prt.h"
#include "common.h"
#include "readstr.h"

#define NGMAX 25
#define NSMAX 20
#define NWMAX 200
#define NLMAX 10
#define PMAX  100
#define LWORDS 30000
#define NCMAX 65535
#define LQUE 400

struct file inrepf, inoptf, outgpf;
#define fip inrepf.fd
#define fio inoptf.fd
#define fog outgpf.fd
#define flip inrepf.flags
#define flio inoptf.flags
#define flog outgpf.isopen

jmp_buf jb;
int fw;

struct word {
    unsigned char *str;
    unsigned int l;
};

unsigned char words[LWORDS];
unsigned int lw = 0;

static void xread(int fd, void *buf, size_t count) {
	ssize_t res = read(fd, buf, count);

	if (res == -1) {
		fprintf(stderr, "read error\n");
		exit(1);
	}
}

static void loceq(x1, x2, cst, lcst, ng, nmax, inv, ind, qmax)
    unsigned int x1, x2, *cst, ng, nmax, *ind;
    unsigned char *inv;
    long lcst, *qmax;
{
    unsigned int que[2 * LQUE], *bq, *eq, *eeq;
    unsigned int j, k, ps, pl, y, u1, u2, *v1, *v2, s1, s2, *z1, *z2,
    *w, v[2];
    long qs, b, e, lq;

    lseek(fw, 0l, SEEK_SET);
    if (x1 > x2) {
	y = x1;
	x1 = x2;
	x2 = y;
    }
    *(cst + (x2 * lcst + 1)) = x1;
    qs = 2 * LQUE * sizeof(*que);
    for (b = e = j = 0, eq = (bq = que) + 2, eeq = que + 2 * LQUE,
	 *bq = x1, *(bq + 1) = x2, lq = 1; lq;
	 lq--, j++, bq += 2) {
	if (lq > *qmax)
	    *qmax = lq;
	if (j == LQUE) {
	    pl = (ps = (e - b > qs) ? qs : (e - b)) / (2 * sizeof(*que));
	    lseek(fw, b, SEEK_SET);
	    xread(fw, que, ps);
	    j = 0;
	    eq = (bq = que) + 2 * pl;
	    b += ps;
	    lseek(fw, e, SEEK_SET);
	}
	for (u1 = *bq; (y = *(v1 = (unsigned int *) (cst + u1 * lcst))) != u1;
	     u1 = y);
	for (u2 = *(bq + 1); (y = *(v2 = (unsigned int *) (cst + u2 * lcst)))
	     != u2; u2 = y);
	*(cst + *bq * lcst) = u1;
	*(cst + *(bq + 1) * lcst) = u2;
	if (u1 != u2) {
	    if (u1 > u2) {
		w = v1;
		v1 = v2;
		v2 = w;
	    }
	    for ((*ind)--, u2 = *v2, u1 = *v2 = *v1, z1 = (v1 += 2),
		 z2 = (v2 += 2), k = 0; k < ng; k++, z1++, z2++)
		if ((s2 = *z2) != nmax) {
		    if (s2 == u2)
			*z2 = u1;
		    else
			*(cst + (s2 * lcst + 2 + *(inv + k))) = u1;
		}
/*   for (k=0; k<n; k++)
    {
    z1=(unsigned int *)(cst+k*lcst);
    printf("%Fp (%2u,%2u)",z1,*z1+1,*(z1+1)+1);
    for (z2=(z1+=2)+ng; z1<z2; z1++)
     if (*z1!=nmax) printf("%3u",*z1+1);
     else printf("  -");
    printf("\n");
    }
   scanf("%c",&k);*/
	    for (z1 = v1, z2 = v2, k = 0; k < ng; k++, z1++, z2++) {
		if ((s1 = *z1) != nmax)
		    if ((s2 = *z2) != nmax && s1 != s2) {
			while ((y = *(v1 = (unsigned int *) (cst +
						   (s1 * lcst + 1)))) != s1)
			    s1 = y;
			while ((y = *(v2 = (unsigned int *) (cst +
						   (s2 * lcst + 1)))) != s2)
			    s2 = y;
			*(cst + (*z1 * lcst + 1)) = s1;
			*(cst + (*z2 * lcst + 1)) = s2;
			if (s1 != s2) {
			    if (s1 > s2) {
				y = s1;
				s1 = s2;
				s2 = y;
				v2 = v1;
			    }
/*       printf("inque %u=%u ",s1+1,s2+1);
       if (eq==eeq) printf("disk=%ld\n",e); else printf("memo=%Fp\n",eq);*/
			    if (eq == eeq) {
				v[0] = s1;
				v[1] = s2;
				e += 2 * sizeof(*v);
				if (write(fw, v, 2 * sizeof(*v)) <= 0) {
				  prt(3,
				    "output error on work file cosen#1.tmp\n");
				  longjmp(jb, 1);
				}
			    } else {
				*eq = s1;
				*(eq + 1) = s2;
				eq += 2;
			    }
			    *v2 = s1;
			    lq++;
			}
		    } else
			*z2 = *z1;
		else
		    *z1 = *z2;
	    }
	}
/*  for (k=0; k<n; k++)
   {
   z1=(unsigned int *)(cst+k*lcst);
   printf("%Fp (%2u,%2u)",z1,*z1+1,*(z1+1)+1);
   for (z2=(z1+=2)+ng; z1<z2; z1++)
    if (*z1!=nmax) printf("%3u",*z1+1);
    else printf("  -");
   printf("\n");
   }
  scanf("%c",&k);*/
    }
}

static void globeq(ng, cst, lcst, n, nmax, nq, q)
    unsigned int ng, *cst, *n, nmax, nq, *q;
    long lcst;
{
    unsigned int n1, i, x, y, *b, *b1, *b2, *b3, *e, k;
    long j;

    for (b = cst, n1 = i = 0; i < *n; i++, b += lcst)
	if (*b == i)
	    *(b + 1) = n1++;
/* for (i=0; i<*n; i++)
  {
  b2=(unsigned int *)(cst+i*lcst);
  if (*b2==i)
   {
   printf("%Fp (%2u,%2u)",b2,*b2+1,*(b2+1)+1);
   for (e=(b2+=2)+ng; b2<e; b2++)
    if (*b2!=nmax) printf("%3u",*b2+1);
    else printf("  -");
   printf("\n");
   }
  }
 scanf("%c",&i);*/
    for (b1 = b = cst, n1 = i = 0; i < *n; i++, b += lcst) {
	if (*b == i)
	    for (b3 = (unsigned int *) b1 + 2, b1 += lcst, n1++,
		 e = (b2 = (unsigned int *) b + 2) + ng,
		 k = 0; b2 < e; b2++, b3++, k++)
		if (*b2 != nmax) {
		    for (x = *b2; (y = *(cst + x * lcst)) != x; x = y);
		    *b3 = *(cst + (x * lcst + 1));
		} else {
		    *b3 = nmax;
		    if (!nq)
			printf("i=%u,j=%u\n", i + 1, k + 1);
		}
    }
    for (j = 0; j < nq; j++) {
	b = q + j;
	*b = *(cst + (*b * lcst + 1));
    }
    for (*n = n1, b = cst, i = 0; i < n1; i++, b += lcst)
	*b = *(b + 1) = i;
/* for (i=0; i<*n; i++)
  {
  b2=(unsigned int *)(cst+i*lcst);
  printf("%Fp (%2u,%2u)",b2,*b2+1,*(b2+1)+1);
  for (e=(b2+=2)+ng; b2<e; b2++)
   if (*b2!=nmax) printf("%3u",*b2+1);
   else printf("  -");
  printf("\n");
  }
 scanf("%c",&i);*/
}

static void outgr(ng, ni, n, cst)
    unsigned int ng, ni, n, *cst;
{
    char str[100];
    unsigned int i, j, x, l, ls, lb, lc, *pg, *pc;
    long lcst;

    lcst = ng + 2;
    ng = (ng + ni) / 2;
    fprintf(fog, "%u\n%u\n", n, ng);
    for (cst += 2, i = 0; i < ng; i++, cst++) {
        lb = lc = ls = 0;
	for (pg = cst, *str = 0, j = 0; j < n; j++, pg += lcst) {
	    for (l = 0, pc = pg, x = j; *pc != n && *pg != j; ) {
		if (x == j) {
		    Sprintf(str + ls, "(%u", x);
		    lb = ls;
		} else {
		    Sprintf(str + ls, ",%u", x);
		    lc = ls + 1;
		}
		ls = strlen(str);
		if(ls > 75) {
		  if(!lb)
		    lb = lc;
		  ls = partline(str, lb, fog);
		  lb = 0;
		}
		x = *pc;
		*pc = n;
		pc = cst + lcst * x;
		l = 1;
	    }
	    if (l) {
		str[ls++] = ')';
		str[ls] = '\0';
	    }
	}
	fprintf(fog, "%s\n", str);
#ifdef _F_ERR
	if (fog->flags & _F_ERR) {
	    prt(3, "output error on file of resulting group\n");
	    longjmp(jb, 1);
	}
#endif
    }
}

static void cosen(ng, ni, inv, nw, ns, word)
    unsigned int ng, ni, nw, ns;
    unsigned char *inv;
    struct word *word;
{
    unsigned int *cst, n, ind, nmax, cmax, i, j, i0, j0, k;
    unsigned int f, x1, x2, y, *z1, *z2, *u, *v, q[3];
    struct word *pw, *ew;
    unsigned char *pg1, *pg2, *eg;
    long lcst, qmax /*, w */;
    int lh;

    lcst = ng + 2;
    nmax = NCMAX;
    if((cst = Getmem(nmax * lcst, unsigned int)) == NULL) {
        prt(3, "Out of memory\n");
	longjmp(jb, 1);
    }
    for (v = (unsigned int *) cst + 2, n = ind = 1, *cst = *(cst + 1) = k = 0;
	 k < ng; k++, v++)
	*v = nmax;
    for (ew = word + nw, lh = qmax = cmax = j0 = i0 = i = 0;
	 i < n || lh; i++) {
	pw = word + (j = (i) ? ns : 0);
	if (i == n) {
	    i = i0;
	    pw = word + (j = j0);
	    lh = 0;
	    printf("lookahead off n=%u\n", ind);
	}
	if (*(cst + i * lcst) == i) {
	    for (; pw < ew && *(cst + i * lcst) == i; pw++, j++) {
		for (x1 = x2 = i, pg2 = eg = (pg1 = pw->str) + pw->l;
		     pg1 < eg; pg1++, x1 = y)
		    if ((y = *(z1 = (unsigned int *) (cst +
					   (x1 * lcst + 2 + *pg1)))) == nmax)
			break;
/*     else
      printf("(%d) %u ",(inv[*pg1]>=*pg1)?*pg1+1:-(int)(inv[*pg1]+1),y+1);
    printf("\n");*/
		for (f = 1; f;) {
		    for (; pg2 > pg1; pg2--, x2 = y)
			if ((y = *(z2 = (unsigned int *) (cst +
			    (x2 * lcst + 2 + *(inv + *(pg2 - 1)))))) == nmax)
			    break;
/*      else
       printf("(%d) %u",
         (inv[*(pg2-1)]>*(pg2-1))?-(int)(*(pg2-1)+1):inv[*(pg2-1)]+1,y+1);
     printf("\n");*/
		    f = 0;
		    if (pg2 == pg1 && x1 != x2)
			loceq(x1, x2, cst, lcst, ng, nmax, inv, &ind, &qmax);
		    if (pg1 < pg2 - 1 && !lh) {
			if (n == nmax) {
			    printf("i=%u,n=%u\n", i + 1, n + 1);
			    q[0] = i;
			    q[1] = x1;
			    q[2] = x2;
			    globeq(ng, cst, lcst, &n, nmax, 3, q);
			    i = q[0];
			    x1 = q[1];
			    x2 = q[2];
			    printf("i=%u,n=%u\n", i + 1, n + 1);
			    if (n == nmax) {
				prt(3, "number of cosets is greater than %u\n",
				    nmax);
				longjmp(jb, 1);
			    }
			    z1 = (unsigned int *) (cst +
						   (x1 * lcst + 2 + *pg1));
			    z2 = (unsigned int *) (cst +
				    (x2 * lcst + 2 + *(inv + *(pg2 - 1))));
			    ind = n;
			    if ((float) n / nmax > 0.7) {
				i0 = i;
				j0 = j + 1;
				lh = 1;
				printf("lookahead on\n");
			    }
			}
			for (*(v = (unsigned int *) (cst + n * lcst)) = n,
			     *(v + 1) = n, u = (v += 2), k = 0;
			     k < ng; k++, v++)
			    *v = nmax;
/*      printf(" %u(%d)=%u %u(%d)=%u\n",
           x1+1,(inv[*pg1]>=*pg1)?*pg1+1:-(int)(inv[*pg1]+1),n+1,
           n+1,(inv[*pg1]>*pg1)?-(int)(*pg1+1):inv[*pg1]+1,x1+1);*/
			*z1 = n;
			*(u + *(inv + *pg1)) = x1;
			z1 = u + *(++pg1);
			x1 = n++;
			ind++;
			f = 1;
		    }
		}
		if ((pg1 != pg2 && !lh) || pg1 == pg2 - 1) {
		    *z1 = x2;
		    *z2 = x1;
/*     printf(" %u(%d)=%u %u(%d)=%u\n",
          x1+1,(inv[*pg1]>=*pg1)?*pg1+1:-(int)(inv[*pg1]+1),x2+1,
          x2+1,(inv[*pg1]>*pg1)?-(int)(*pg1+1):inv[*pg1]+1,x1+1);*/
		}
		if (ind > cmax)
		    cmax = ind;
/*    for (k=0; k<n; k++)
     {
     z1=(unsigned int *)(cst+k*lcst);
     printf("%Fp (%2u)",z1,*z1+1);
     for (z2=(z1+=2)+ng; z1<z2; z1++)
      if (*z1!=nmax) printf("%3u",*z1+1);
      else printf("  -");
     printf("\n");
     }
    scanf("%c",&u);*/
	    }
	}
    }
    prt(3, "index=%u, cmax=%u, qmax=%ld\n", ind, cmax, qmax);
    if (flog) {
	globeq(ng, cst, lcst, &n, nmax, 0, q);
	outgr(ng, ni, n, cst);
    }
    free(cst);
}

static int inpword(ng, ni, inv, ref, l, poserr)
    unsigned int ng, ni, *l, *poserr;
    unsigned char *inv, **ref;
{
    char s;
    unsigned int n, j, r, lev, p;
    unsigned char x, z, *pw, *pw1, *ew, *ew1, *br[NLMAX];

    readstr(fip, flip);
    lev = 0;
    n = (ng + ni) / 2;
    *ref = pw = words + lw;
    ew = words + LWORDS;
    for (r = 1,  x = *l = j = 0; (s = nxtsym()); j++)
	switch (r) {
	case 1:
	    if (s == '-') {
		x = 1;
		r = 2;
		break;
	    }
	    if (s > '0' && s <= '9' && (z = s - '0') <= n) {
		r = 3;
		break;
	    }
	    if (s == '(' && lev < NLMAX) {
		br[lev++] = pw;
		break;
	    }
	    *poserr = j;
	    return 0;
	case 2:
	    if (s > '0' && s <= '9' && (z = s - '0') <= n) {
		r = 3;
		break;
	    }
	    *poserr = j;
	    return 0;
	case 3:
	    if (s >= '0' && s <= '9' &&
		(z = 10 * z + (s - '0')) <= n)
		break;
	    if (s == ',' || (s == ')' && lev)) {
		if (pw == ew) {
		    prt(3, "not enough space for words\n");
		    longjmp(jb, 1);
		} else {
		    *(pw++) = (x) ? inv[z - 1] : z - 1;
		    x = p = 0;
		    r = (s == ',') ? 1 : 4;
		    (*l)++;
		    break;
		}
	    }
	    *poserr = j;
	    return 0;
	case 4:
	    if (s >= '0' && s <= '9' &&
		(p = 10 * p + (s - '0')) <= PMAX)
		break;
	    if (s == ',' || (s == ')' && lev > 1)) {
		if (p > 1) {
		    if ((ew - pw) / (unsigned int) (pw - br[--lev]) < (--p)) {
			prt(3, "not enough space for words\n");
			longjmp(jb, 1);
		    }
		    for (ew1 = pw; p; p--)
			for (pw1 = br[lev]; pw1 < ew1; (*l)++)
			    *(pw++) = *(pw1++);
		}
		if (s == ')') {
		    p = 0;
		    break;
		}
		r = 1;
		break;
	    }
	    *poserr = j;
	    return 0;
	}
    if (r == 3) {
	if (pw == ew) {
	    prt(3, "not enough space for words\n");
	    longjmp(jb, 1);
	}
	*(pw++) = (x) ? inv[z - 1] : z - 1;
	(*l)++;
    } else if (r == 4) {
	if (p > 1) {
	    if ((unsigned int) (ew - pw) / (unsigned int) (pw - br[--lev]) <
		(--p)) {
		prt(3, "not enough space for words\n");
		longjmp(jb, 1);
	    }
	    for (ew1 = pw; p; p--)
		for (pw1 = br[lev]; pw1 < ew1; (*l)++)
		    *(pw++) = *(pw1++);
	}
    } else {
	*poserr = j - 1;
	return 0;
    }
    lw += *l;
    return 1;
}

static void input(ng, ni, inv, nw, ns, word)
    unsigned int *ng, *ni, *nw, *ns;
    unsigned char *inv;
    struct word *word;
{
    unsigned int n, nimin, i, l, poserr;
    unsigned char *ref;

    while (1) {
	if (flip)
	    prt(3, "number of generators of group (upto %u):\n", NGMAX);
	readstr(fip, flip);
	if (inpnum(&n) && n > 0 && n <= NGMAX)
	    break;
	prt(3, "error\n");
	if (!(flip & KBDBIT))
	    longjmp(jb, 1);
    }
    nimin = (2 * n > NGMAX) ? (2 * n - NGMAX) : 0;
    while (1) {
	if (flip)
	    prt(3, "number of involutions (from %u upto %u):\n", nimin, n);
	readstr(fip, flip);
	if (inpnum(ni) && *ni >= nimin && *ni <= n)
	    break;
	prt(3, "error\n");
	if (!(flip & KBDBIT))
	    longjmp(jb, 1);
    }
    for (*ng = 2 * n - *ni, i = 0; i < n; i++)
	if (i < *ni)
	    inv[i] = i;
	else {
	    inv[i] = i + n - *ni;
	    inv[i + n - *ni] = i;
	}
    while (1) {
	if (flip)
	    prt(3, "number of generators of subgroup (upto %u):\n", NSMAX);
	readstr(fip, flip);
	if (inpnum(ns) && *ni <= NSMAX)
	    break;
	prt(3, "error\n");
	if (!(flip & KBDBIT))
	    longjmp(jb, 1);
    }
    if (flip && *ns)
	prt(3, "generators of subgroup:\n");
    for (i = 0; i < *ns; i++)
	while (1) {
	    if (flip)
		prt(3, "%d.", i + 1);
	    if (inpword(*ng, *ni, inv, &ref, &l, &poserr)) {
		word[i].str = ref;
		word[i].l = l;
		break;
	    }
	    prt(3, "error in position %d\n", poserr);
	    if (!(flip & KBDBIT))
		longjmp(jb, 1);
	}
    while (1) {
	if (flip)
	    prt(3, "number of relators (upto %u):\n", NWMAX - *ns);
	readstr(fip, flip);
	if (inpnum(nw) && *nw > 0 && *nw <= NWMAX - *ns)
	    break;
	prt(3, "error\n");
	if (!(flip & KBDBIT))
	    longjmp(jb, 1);
    }
    if (flip)
	prt(3, "relators:\n");
    for (i = 0; i < *nw; i++)
	while (1) {
	    if (flip)
		prt(3, "%d.", i + 1);
	    if (inpword(*ng, *ni, inv, &ref, &l, &poserr)) {
		word[i + *ns].str = ref;
		word[i + *ns].l = l;
		break;
	    }
	    prt(3, "error in position %d\n", poserr);
	    if (!(flip & KBDBIT))
		longjmp(jb, 1);
	}
    *nw += *ns;
    if (flip)
	prt(3, "\n");
}

static void openf(na, pa)
    char **pa;
    int na;
{
    if(!openfile("input of co-presentation from %s\n",
		 &inrepf, (na > 2) ? pa[2] : "*", "rep", "rt"))
      longjmp(jb, 1);

    if(!openfile("input of options from %s\n",
		 &inoptf, (na > 3) ? pa[3] : "*", "opt", "rt"))
      longjmp(jb, 1);

    if (na > 4) {
	if(!openfile("output of permutation representation to file %s\n",
		     &outgpf, pa[4], "gen", "wt"))
	  longjmp(jb, 1);
    } else
	flog = 0;

    if ((fw = open("cosen#1.tmp", O_RDWR | O_CREAT, 0666)) < 0) {
	prt(3, "open error on work file cosen#1.tmp : %s\n", strerror(errno));
	longjmp(jb, 1);
    }
    prt(3, "\n");
}

void cleanup()
{
    if (!(flip & KBDBIT))
	fclose(fip);
    if (!(flio & KBDBIT))
	fclose(fio);
    if (flog)
	fclose(fog);
    close(fw);
    unlink("cosen#1.tmp");
}

void Main(na, pa)
    char **pa;
    int na;
{
    unsigned int ng, ni, nw, ns;
    unsigned char inv[NGMAX];
    struct word word[NWMAX];

    prt(3, "\n* coset enumeration *\n\n");
    openf(na, pa);
    input(&ng, &ni, inv, &nw, &ns, word);
    cosen(ng, ni, inv, nw, ns, word);
}
