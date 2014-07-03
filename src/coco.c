/*
 * COCO - Unix version 1.0 - aeb - 920530
 *
 * coco.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <sys/wait.h>
#include "def.h"
#include "prt.h"

#define NLIST 7

#define NSTR 100
#define NFN  100
char folname[100];
FILE *fol;

char *coco;

/* commands in the default order */
char *list[NLIST] = { "cosen", "ind", "cgr", "inm", "sub", "aut", "exp"};
char *inext[NLIST] = { "rep",  "gen", "gen", "cgr", "nrs", "sub", "gen"};
char *outext[NLIST] = {"gen",  "gen", "cgr", "nrs", "sub", "",    "gen"};


static int help(char *str)
{
    int l;
    char cmdbuf[100];
    char argbuf[100];

    l = strlen(str) - 1;
    if (str[l] != '?')
        return 0;
    if (l+1 >= 100) {
	prt(3, "help arg too long\n");
	return(1);
    }
    strcpy(argbuf, str);
    argbuf[l] = 0;
    if (coco)
      Sprintf(cmdbuf, "more %s/doc/%s.doc", coco, l ? argbuf : "coco");
    else
      Sprintf(cmdbuf, "more %s.doc", l ? argbuf : "coco");
    if (system(cmdbuf)) {
	prt(3, "The help command returned nonzero status\n");
	return(1);
    }
    return 1;
}

static void setlistingfile(fn, mode)
    char *fn, *mode;
{
    if (fol != NULL)
        (void) fclose(fol);
    if ((fol = fopen(fn, mode)) == NULL) {
	printf("open error on file %s : %s\n",
	       fn, strerror(errno));
	exit(1);
    }
    Strcpy(folname, fn);
    prt(3,
	"\n*** system CO-CO for construction of coherent configurations ***\n\n");
    prt(3, "output listing to file %s\n", fn);
}

/* leave the command in str, but put pointers to its args in arg[] */
static void inpcom(str, na, arg)
    char str[], *arg[];
    int *na;
{
    unsigned int j, l;

    prt(3, "\nCOCO>> ");
    if (!fgets(str, NSTR, stdin)) {
      prt(3, "end of input\n");
      exit(0);
    }
    prt(2, "%s", str);

    l = strlen(str);
    str[--l] = 0;            /* remove \n */
    while(str[l-1] == ' ')   /* and final spaces */
        str[--l] = 0;
    str[l++] = '\n';         /* add back in again */
    for (*na = 1, j = 0; j < l; j++)
	if (str[j] == ' ' || str[j] == '\n') {
	    str[j] = '\0';
	    arg[++(*na)] = &str[j + 1];
	}
    arg[(*na)--] = 0;
}

static void spawn(prog, arg) char *prog, *arg[];
{
    int status;
    extern char *getenv();

    (void) fclose(fol);
    arg[0] = prog;
    if (!fork()) {
      execv(prog, arg);
      /* it failed - try another directory */
      if (*prog != '/' && coco != NULL) {
	char buf[1000];
	Sprintf(buf, "%s/bin/%s", coco, prog);
	execv(buf,arg);
      }
      Fprintf(stderr, "cannot exec %s : %s\n", prog, strerror(errno));
      exit(1);
    }
    wait(&status);
    if ((fol = fopen(folname, "a")) == NULL)
      Fprintf(stderr, "cannot (re)open listing file\n");
}

int main(int na, char *pa[])
{
    char str[NSTR], *arg[10];
    int i, narg;
    char lastof[NFN];     /* last readable of - used by look */
    char lastbase[NFN];
    char lastext[10];

    (void) signal(SIGINT, SIG_IGN);
    coco = getenv("COCO");
    arg[1] = (na > 1) ? pa[1] : "coco.res";
    setlistingfile(arg[1],"w");

    Strcpy(lastof, "/dev/null");
    lastbase[0] = 0;

    while (1) {
        arg[1] = folname;
	inpcom(str, &narg, arg);
	if (!*str)
	  continue; /* might try a default action */
	if (help(str))
	  continue;
	for (i = 0; i < NLIST; i++)
	  if (strcmp(str, list[i]) == 0)
	    break;
	
	if (i < NLIST) {
	  if (narg < 2) {
	    /* try a default action */
	    if (*lastbase && strcmp(inext[i], lastext) == 0 &&
	     strcmp(inext[i], outext[i]) != 0) {
	       if (i == 5) {      /* "aut" has somewhat different syntax */
		 arg[2] = arg[3] = lastbase;
		 narg = 3;
	       } else {
		 arg[2] = arg[4] = lastbase;
		 arg[3] = "*";
		 narg = 4;
	       }
	     } else {
	       prt(3, "some argument is required here\n");
	       continue;
	     }
	  }
	  if (narg >= 4) {
	    Strcpy(lastbase, arg[4]);
	    Strcpy(lastext, outext[i]);
	    if (strcmp(outext[i],"cgr") != 0)
	      Sprintf(lastof, "%s.%s", arg[4], outext[i]);
	  }
	  spawn(list[i], arg);
	}
	else if (strcmp(str, "more") == 0)
	  spawn("/usr/ucb/more", arg+1);
	else if (strcmp(str, "look") == 0) {
	  if (narg > 1) {
	    prt(3, "\nDo not use `look' with arguments");
	    prt(3, " - use `more' instead\n");
	  } else {
	    arg[1] = lastof;
	    spawn("/usr/ucb/more", arg);
	  }
	}
	else if (strcmp(str, "res") == 0)
	  spawn("/usr/ucb/more", arg);
	else {
	  if (strcmp(str, "new") == 0) {
	    prt(3, "\nchange of file for output listing\n");
	    arg[1] = ((narg > 1) ? arg[2] : "coco.res");
	    setlistingfile(arg[1],"a");
	  } else if (strcmp(str, "end") == 0) {
	    prt(3, "\n* end of CO-CO *\n");
	    break;
	  } else if (strcmp(str, "help") == 0) {
	    (void) help("?");
	  } else
	    prt(3, "\nunrecognized command\n");
	}
    }
    return(0);
}
