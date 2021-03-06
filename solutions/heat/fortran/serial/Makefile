COMP=cray

COMMONDIR=../../common

ifeq ($(COMP),cray)
FC=ftn
CC=cc
FCFLAGS=-O3 -g -R bcs
CCFLAGS=-O3 -g -R bcs -I/appl/opt/libpng/include -I$(COMMONDIR)
LDFLAGS=-L/appl/opt/libpng/lib
LIBS=-lpng -lz
endif

ifeq ($(COMP),gnu)
FC=gfortran
CC=gcc
FCFLAGS=-O3 -Wall -march=native
CCFLAGS=-O3 -Wall -I$(COMMONDIR)
LDFLAGS=
LIBS=-lpng
endif

ifeq ($(COMP),intel)
FC=ftn
CC=cc
FCFLAGS=-O3 -fno-omit-frame-pointer -masm=intel -fverbose-asm -g -ggdb
CCFLAGS=-O3 -fno-omit-frame-pointer -masm=intel -fverbose-asm -g -ggdb -I/appl/opt/libpng/include -I$(COMMONDIR)
LDFLAGS=-L/appl/opt/libpng/lib -fno-omit-frame-pointer -masm=intel -fverbose-asm -g -ggdb
LIBS=-lpng -lz
endif

EXE=heat_serial
OBJS=main.o heat_mod.o core.o setup.o utilities.o io.o pngwriter_mod.o heat_core.o
OBJS_PNG=$(COMMONDIR)/pngwriter.o


all: $(EXE)

$(COMMONDIR)/pngwriter.o: $(COMMONDIR)/pngwriter.c $(COMMONDIR)/pngwriter.h
core.o: core.F90 heat_mod.o
utilities.o: utilities.F90 heat_mod.o
io.o: io.F90 heat_mod.o pngwriter_mod.o 
setup.o: setup.F90 heat_mod.o utilities.o io.o
pngwriter_mod.o: pngwriter_mod.F90 heat_mod.o
main.o: main.F90 heat_mod.o core.o io.o setup.o utilities.o heat_core.o

$(EXE): $(OBJS) $(OBJS_PNG)
	$(FC) $(FCFLAGS) $(OBJS) $(OBJS_PNG) -o $@ $(LDFLAGS) $(LIBS)

%.o: %.F90
	$(FC) $(FCFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CCFLAGS) -c $< -o $@

.PHONY: clean
clean:
	-/bin/rm -f $(EXE) a.out *.o *.mod *.png *~ ../../common/pngwriter.o
