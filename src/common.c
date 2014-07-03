/*
 * COCO - Unix version 1.0 - aeb - 920530
 *
 * common.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <setjmp.h>
#include <signal.h>
#include <string.h>
#include "def.h"
#include "prt.h"
#include "common.h"

extern char *getenv();
extern jmp_buf jb;

#define Printf  printf

FILE *fol;

static void intrup()
{
    prt(3, "\nusers break\n");
    longjmp(jb, 1);
}

int openfile(char *text, struct file *file, char *name, char *ext, char *mode)
{
    char buf[1000];
    int l, excl;
    char *coco;

    if(file->isopen) {
      prt(3, "openfile called for already open file?\n");
      return(0);
    }

    file->flags = 0;
    excl = 0;
    l = strlen(name) - 1;
    if(name[l] == '!') {
      excl++;
      name[l] = 0;
      file->flags = VERBOSE;
    }
    if(!excl && strcmp(name,"*") == 0) {
      if(*mode == 'r') {
	Strcpy(buf, "<stdin>");
	file->fd = stdin;
	file->flags |= (KBDBIT + VERBOSE);
      } else
	return(1); /* on output * means: no file */
    } else {
      Sprintf(buf, "%s.%s", name, ext);
      if((file->fd = fopen(buf, mode)) == NULL) {
	if((file->fd = fopen(name, mode)) == NULL) {
	  if(*mode == 'r' && *name != '/' && (coco = getenv("COCO"))) {
	    Sprintf(buf, "%s/%s/%s.%s", coco, ext, name, ext);
	    if((file->fd = fopen(buf, mode)) != NULL)
	      goto foundit;
	    Sprintf(buf, "%s/%s/%s", coco, ext, name);
	    if((file->fd = fopen(buf, mode)) != NULL)
	      goto foundit;
	  }
	  prt(3, "open error on file %s : %s\n", name, strerror(errno));
	  if(excl)
	    name[l] = '!';
	  return(0);
	}
	Strcpy(buf, name);
      }
    }
  foundit:
    file->isopen = 1;
    if(*mode == 'w')
      file->flags |= EXISTS;
    file->name = malloc((unsigned)strlen(buf)+1);
    Strcpy(file->name, buf);
    if(text)
      prt(3, text, buf);
    if(excl)
      name[l] = '!';
    return(1);
}

int partline(char *str, unsigned int cutpt, FILE *f)
{
    int l;
    char s, *cp;

    if((l = strlen(str)) > 75) {
        cp = str + cutpt;
	s = *cp;
	*cp++ = 0;
	Fprintf(f, "%s*\n", str);
	if(f == stdout)
	  Fprintf(fol, "%s*\n", str);
	*str++ = s;
	bcopy(cp, str, l - (int) cutpt);
	return(l-cutpt);
    } else
        return(l);
}

void read0(int f, char *b, int l)
{
    int r;
    r = read(f, b, l);
    if(r == l)
      return;
    if(r < 0)
      prt(3, "read error on binary file : %s\n", strerror(errno));
    else
      prt(3, "unexpected end-of-file on binary file\n");
    longjmp(jb, 1);
}

int main(int na, char **pa)
{
    char *fn;
    fn = (na > 1) ? pa[1] : "coco.res";
    if ((fol = fopen(fn, "a")) == NULL) {
	Fprintf(stderr, "open error on file %s : %s\n", fn, strerror(errno));
	return(1);
    }
    if (!setjmp(jb)) {
        signal(SIGINT, intrup);
        Main(na, pa);
    }
    cleanup();
    fclose(fol);
    return(0);
}
