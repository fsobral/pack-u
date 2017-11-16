TANGOLIB = /opt/tango/algencan-3.0.0/lib

PACKPROBLEM = packminc

# Directories

PACKUDIR = $(CURDIR)
BUILDDIR = $(PACKUDIR)/build
TESTDIR = $(PACKUDIR)/tests

# Compiler options

CC  = gcc
FC  = gfortran
FCC = 

lib: libpacku.a

libpacku.a: packdat.o items.o containers.o
	ar crv $@ $^

packu: $(PACKPROBLEM).o packdat.o
	gfortran $^ -L$(TANGOLIB) -lalgencan -o $@
	mkdir -p $(BUILDDIR)
	mv -f $@ $(BUILDDIR)/.

tests: lib
	$(MAKE) -C $(TESTDIR) clean run

%.o: %.f90
	gfortran -c $^

packminc.o: packminc.f90 packdat.mod
	gfortran -c $<

packminw.o: packminw.f90 packdat.mod
	gfortran -c $<

packdat.mod: packdat.f90
	gfortran -c $^

items.mod: items.f90
	gfortran -c $^

containers.mod: containers.f90 items.mod 
	gfortran -c $<

clean:
	rm -f *.mod *.o solution.csv solution.pdf \
	sol[0-9][0-9][0-9].* build/*
	$(MAKE) -C $(TESTDIR) clean
