/*
 * COCO - Unix version 1.0 - aeb - 920530
 *
 * readstr.c
 */
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include "def.h"
#include "prt.h"
#include "readstr.h"

#define NSTR 250   /* arbitrary, but > 5 for the benefit of strequal */
static char str0[NSTR], *sp;
static FILE *f0;
static int len0, ofl0;

int strequal(char *str)
{
    int l = strlen(str);
    if (l >= NSTR) {
      prt(3, "internal error - strequal for too large a string\n");
      longjmp(jb, 1);
    }
    return(strcmp(str, str0) == 0);
}

static void readmore()
{
    if (fgets(str0, len0, f0) == NULL) {
      prt(ofl0 | 1, "end of input.\n");
      longjmp(jb, 1);
    }
    prt(ofl0, "%s", str0);
}

char nxtsym() {       /* read next symbol of line, but not past \n */
  if (!sp) {
    prt(3, "internal error - nxtsym before readstr?\n");
    longjmp(jb, 1);
  }
  while(1) {
    while(*sp == ' ')
      sp++;
    if (!*sp || (*sp == '*' && sp[1] == '\n')) {
      readmore();
      sp = str0;
    } else
      break;
  }
  if (*sp == '\n') {
    sp = 0;
    return 0;
  }
  return(*sp++);
}

void readstr(FILE *f, int fl)           /* read (part of) new line */
{
    sp = str0;
    len0 = NSTR;
    f0 = f;
    ofl0 = ((fl==0) ? 0 : (fl==VERBOSE) ? 3 : /* VERBOSE+KBDBIT */ 2);
    readmore();
}

int inpnum(unsigned int *x)      /* read a line containing a single number */
{
    char s;

    for (*x = 0; (s = nxtsym());  ) {
      if (s < '0' || s > '9') {
	prt(3, "inpnum: bad character %c\n", s);
	return (0);
      }
      *x = 10 * (*x) + (s - '0');
    }
    return (1);
}

/* the old readstr() is still needed in ind.c */
/* read the entire line into one string */
int read1str(FILE *f, int fl, char *str, int len)
{
    char s;
    unsigned int i, j, l, ofl;
    ofl = ((fl==0) ? 0 : (fl==VERBOSE) ? 3 : /* VERBOSE+KBDBIT */ 2);
    for (i = 0;;) {
	if (fgets(str + i, len - (int)i, f) == NULL) {
	    prt(ofl | 1, "end of input.\n");
	    longjmp(jb, 1);
	}
	l = strlen(str);
	prt(ofl, "%s", str+i);
	for (j = i; j < l; j++) {
	    if ((s = str[i++] = str[j]) == ' ')
		i--;               /* remove spaces */
	    if (s == '\n')
		break;
	}
	if (j == l) {              /* unfortunately, no more room */
	    prt(ofl, "\n");        /* act as if '\n' seen */
	    prt(3, "Input line too long.\n");
	    break;
	}
	if (str[(--i) - 1] == '*') /* continuation line follows */
	    i--;
	else
	    break;
    }
    str[i] = '\0';
    return(i);     /* return the string length (in fact no longer used) */
}

