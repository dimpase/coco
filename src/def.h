#include <unistd.h>      /* for SEEK_SET */

struct file {
  char *name;
  FILE *fd;
  int isopen;
  int flags;
#define EXISTS 01   /* for output files only */
#define VERBOSE 02
#define KBDBIT 04   /* for input files only */
};

struct ggen {
  unsigned int n, ng, no, *gens;
};

extern char nxtsym();

#define Strcpy  strcpy
#define Sprintf sprintf
#define Fprintf fprintf
#define getmem  calloc
#define Getmem(n,typ) (typ *) getmem(n, sizeof(typ))

extern void read0(int f, char *b, int l);
extern int partline(char *str, unsigned int cutpt, FILE *f);
extern void cleanup(void);
extern void Main(int ac, char **av);
