/*
 * COCO - Unix version 1.0 - aeb - 920530
 *
 * prt.c
 */
#include <stdio.h>
#include "prt.h"

void prt(unsigned int r, const char *s, ...)
{
    extern FILE *fol;
    va_list p;

    if (r & 1) {
	    va_start(p, s);
	    vfprintf(stdout, s, p);
	    va_end(p);
    }
    if ((r & 2) && fol != NULL) {
	    va_start(p, s);
	    vfprintf(fol, s, p);
	    va_end(p);
    }
}

