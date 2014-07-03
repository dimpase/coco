extern jmp_buf jb;

extern int strequal(char *str);
extern char nxtsym(void);
extern void readstr(FILE *f, int fl);
extern int inpnum(unsigned int *x);
extern int read1str(FILE *f, int fl, char *str, int len);
