# COCO Makefile
#
# It seems that GNU make doesn't like the SUN $@ construct, while
# SUN make doesn't like the GNU extensions. So, all dependencies
# are written out in full here.

CFLAGS=-O -g
ALL=aut cgr coco cosen crein exp ind inm sub
OBJ=aut.o cgr.o coco.o common.o cosen.o exp.o ind.o ingp.o inm.o \
	outgp.o readstr.o sub.o

all: $(ALL)

$(OBJ): def.h

coco: coco.o prt.o
	$(CC) $(CFLAGS) -o coco coco.o prt.o

aut: aut.o common.o readstr.o prt.o
	$(CC) $(CFLAGS) -o aut aut.o common.o readstr.o prt.o

cosen: cosen.o common.o readstr.o prt.o
	$(CC) $(CFLAGS) -o cosen cosen.o common.o readstr.o prt.o

sub: sub.o common.o readstr.o prt.o
	$(CC) $(CFLAGS) -o sub sub.o common.o readstr.o prt.o

cgr: cgr.o common.o ingp.o readstr.o prt.o
	$(CC) $(CFLAGS) -o cgr cgr.o common.o ingp.o readstr.o prt.o

GGOBJ=common.o ingp.o outgp.o prt.o readstr.o
exp: exp.o $(GGOBJ)
	$(CC) $(CFLAGS) -o exp exp.o $(GGOBJ)

ind: ind.o $(GGOBJ)
	$(CC) $(CFLAGS) -o ind ind.o $(GGOBJ)

INMOBJ=inm.o common.o outgp.o prt.o readstr.o
inm: $(INMOBJ)
	$(CC) $(CFLAGS) -o inm $(INMOBJ)

crein: crein.o
	$(CC) $(CFLAGS) -o crein crein.o -lm

install: $(ALL)
	cp $(ALL) ../bin
	cp ../bin/coco $(HOME)/bin

clean:
	/bin/rm -f core *.o *~ $(ALL) coco.res

lint:	lint0 lint1 lint2 lint3 lint4 lint5 lint6 lint7

lint0:
	lint coco.c prt.c

lint1:
	lint aut.c common.c readstr.c prt.c

lint2:
	lint cgr.c common.c ingp.c readstr.c prt.c

lint3:
	lint cosen.c common.c readstr.c prt.c

lint4:
	lint exp.c common.c ingp.c readstr.c prt.c outgp.c

lint5:
	lint ind.c common.c ingp.c readstr.c prt.c outgp.c

lint6:
	lint inm.c common.c readstr.c prt.c outgp.c

lint7:
	lint sub.c common.c readstr.c prt.c
